Class {
	#name : 'RwClsExtensionAuditTool',
	#superclass : 'RwClassTool',
	#category : 'Rowan-Tools-Core'
}

{ #category : 'other' }
RwClsExtensionAuditTool >> _addAll: aColl to: aDict for: aName [

	"aDict at: aName ifAbsentPut: [aColl]."
	
aDict addAll: aColl

]

{ #category : 'other' }
RwClsExtensionAuditTool >> auditLoadedClassExtenstion: aLoadedClass [
"look for methods compiled into class without Rowan API"
| res  aBehavior |

	res := Array new.
	(GsSession currentSession resolveSymbol: aLoadedClass name) 
		ifNil: [self _addAll: { 'Compiled class does not exists '-> aLoadedClass name} to: res for: aLoadedClass name] "there is no matching actual class for loaded class"
		ifNotNil: [:bhv | | selectors |

			aBehavior := bhv value.
			(aBehavior includesCategory: aLoadedClass loadedPackage asExtensionName) ifTrue: [
			selectors := aBehavior selectorsIn: aLoadedClass loadedPackage asExtensionName.
				(selectors reject: [:e | aLoadedClass includesSelector: e isMeta: false]) do: [:aSelector |
								self _addAll: { aSelector -> 'Missing loaded extension method' ->  aSelector } to: res for: aLoadedClass name.
						]
			].
			(aBehavior class includesCategory: aLoadedClass loadedPackage asExtensionName) ifTrue: [
					selectors := aBehavior class selectorsIn: aLoadedClass loadedPackage asExtensionName.
						( selectors reject: [:e | aLoadedClass includesSelector: e isMeta: true]) do: [:aSelector |
								self _addAll: {aSelector -> 'Missing loaded extensions classmethod: '   } to: res for: aLoadedClass name.
						]						
			]
	].
"look for methods removed outside of Rowan API"
	aBehavior notNil ifTrue: [
		aLoadedClass 
			loadedInstanceMethodsDo: [:loadedProject :loadedPackage :loadedClass :aLoadedMethod | 
				(aBehavior compiledMethodAt: aLoadedMethod name) ifNil: [self _addAll: {aLoadedMethod -> 'Missing compiled method: '   } to: res for: aLoadedClass name.
"Transcript show: 'Missing ', aLoadedMethod printString"]]
			loadedClassMethodsDo: [:loadedProject :loadedPackage :loadedClass :aLoadedMethod |
				(aBehavior class compiledMethodAt: aLoadedMethod name) ifNil: [self _addAll: { aLoadedMethod -> 'Missing compiled method: '    } to: res for: aLoadedClass name.
"Transcript show: 'Missing ', aLoadedMethod printString"]]
	].
	^res

]
