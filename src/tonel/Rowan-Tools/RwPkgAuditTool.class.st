Class {
	#name : 'RwPkgAuditTool',
	#superclass : 'RwPackageTool',
	#category : 'Rowan-Tools'
}

{ #category : 'other' }
RwPkgAuditTool >> auditForPackageName: packageName [

	| loadedPackage  res|
	res := Array new.
	loadedPackage := Rowan image loadedPackageNamed: packageName.
	loadedPackage 
			loadedClassesDo: [:aLoadedClass |  res addAll: (self auditLoadedClass: aLoadedClass)]				
			loadedClassExtenstionsDo: [:aLoadedClass | res addAll: (self auditLoadedClassExtenstion: aLoadedClass) ] .
	^res

]

{ #category : 'other' }
RwPkgAuditTool >> auditLoadedClass: aLoadedClass [
"look for methods compiled into class without Rowan API"
| res x aBehavior |

	res := Array new.
	(GsSession currentSession resolveSymbol: aLoadedClass name) 
		ifNil: [res add: aLoadedClass] "there is no matching Class for LoadedClass"
		ifNotNil: [:bhv | 
						(x := (aBehavior := bhv value) selectors reject: [:e | aLoadedClass includesSelector: e isMeta: false]) do: [:aSelector |
								res add: (aBehavior compiledMethodAt: aSelector ).
Transcript show: 'Missing ', aSelector printString
						].
						(x := (aBehavior := bhv value ) class  selectors reject: [:e | aLoadedClass includesSelector: e isMeta: true]) do: [:aSelector |
								res add: (aBehavior class compiledMethodAt: aSelector  ).
Transcript show: 'Missing ', aSelector printString
						].
	].
"look for methods removed outside of Rowan API"
	aBehavior notNil ifTrue: [
		aLoadedClass 
			loadedInstanceMethodsDo: [:aLoadedMethod | (aBehavior compiledMethodAt: aLoadedMethod name) ifNil: [res add: aLoadedMethod.
Transcript show: 'Missing ', aLoadedMethod printString]]
			loadedClassMethodsDo: [:aLoadedMethod |(aBehavior class compiledMethodAt: aLoadedMethod name) ifNil: [res add: aLoadedMethod.
Transcript show: 'Missing ', aLoadedMethod printString]]
	].
	^res

]

{ #category : 'other' }
RwPkgAuditTool >> auditLoadedClassExtenstion: aLoadedClass [
"look for methods compiled into class without Rowan API"
| res  aBehavior |

	res := Array new.
	(GsSession currentSession resolveSymbol: aLoadedClass name) 
		ifNil: [res add: aLoadedClass]"there is no matching actual class for loaded class"
		ifNotNil: [:bhv | aBehavior := bhv value ].
"look for methods removed outside of Rowan API"
	aBehavior notNil ifTrue: [
		aLoadedClass 
			loadedInstanceMethodsDo: [:aLoadedMethod | (aBehavior compiledMethodAt: aLoadedMethod name) ifNil: [res add: aLoadedMethod.
Transcript show: 'Missing ', aLoadedMethod printString]]
			loadedClassMethodsDo: [:aLoadedMethod |(aBehavior class compiledMethodAt: aLoadedMethod name) ifNil: [res add: aLoadedMethod.
Transcript show: 'Missing ', aLoadedMethod printString]]
	].
	^res

]


