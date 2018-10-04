Class {
	#name : 'RwClsAuditTool',
	#superclass : 'RwClassTool',
	#category : 'Rowan-Tools-Core'
}

{ #category : 'other' }
RwClsAuditTool >> _addAll: aColl to: aDict for: aName [

	"aDict at: aName ifAbsentPut: [aColl]."
	aDict addAll: aColl

]

{ #category : 'other' }
RwClsAuditTool >> _addAllMissing: aColl to: aDict for: aName [

	aDict at: aName ifAbsentPut: [aColl].

]

{ #category : 'other' }
RwClsAuditTool >> auditLoadedClass: aLoadedClass [
"look for methods compiled into class without Rowan API"
| res x aBehavior |

	res := Array new.
	(GsSession currentSession resolveSymbol: aLoadedClass name) 
		ifNil: [self _addAll: { 'Class does not exists ' -> aLoadedClass name} to: res for: aLoadedClass name] "there is no matching Class for LoadedClass"
		ifNotNil: [:bhv | 
						(x := (aBehavior := bhv value) selectors reject: [:e | aLoadedClass includesSelector: e isMeta: false]) do: [:aSelector |
								self _addAll: { 'Missing loaded method ' ->  aSelector } to: res for: aLoadedClass name.
"Transcript show: 'Missing ', aSelector printString"
						].
						(x := (aBehavior := bhv value ) class  selectors reject: [:e | aLoadedClass includesSelector: e isMeta: true]) do: [:aSelector |
								self _addAll: {'Missing loaded classmethod: '->  aSelector   } to: res for: aLoadedClass name.
"Transcript show: 'Missing ', aSelector printString"
						].
	].
"look for methods removed outside of Rowan API"
	aBehavior notNil ifTrue: [
		aLoadedClass 
			loadedInstanceMethodsDo: [ :loadedProject :loadedPackage :loadedClass :aLoadedMethod | (aBehavior compiledMethodAt: aLoadedMethod name) 
					ifNil: [self _addAll: { 'Missing compiled method: ' -> aLoadedMethod  } to: res for: aLoadedClass name.
"Transcript show: 'Missing ', aLoadedMethod printString"]]
			loadedClassMethodsDo: [:loadedProject :loadedPackage :loadedClass :aLoadedMethod |(aBehavior class compiledMethodAt: aLoadedMethod name) 
					ifNil: [self _addAll: { 'Missing compiled classmethod ' -> aLoadedMethod } to: res for: aLoadedClass name.
"Transcript show: 'Missing ', aLoadedMethod printString"]]
	].
	^res
]
