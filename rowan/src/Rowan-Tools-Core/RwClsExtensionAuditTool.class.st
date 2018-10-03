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
		ifNil: [res add: aLoadedClass]"there is no matching actual class for loaded class"
		ifNotNil: [:bhv | aBehavior := bhv value ].
"look for methods removed outside of Rowan API"
	aBehavior notNil ifTrue: [
		aLoadedClass 
			loadedInstanceMethodsDo: [:aLoadedMethod | 
				(aBehavior compiledMethodAt: aLoadedMethod name) ifNil: [self _addAll: { 'Missing compiled method: ', aLoadedMethod name } to: res for: aLoadedClass name.
"Transcript show: 'Missing ', aLoadedMethod printString"]]
			loadedClassMethodsDo: [:aLoadedMethod |
				(aBehavior class compiledMethodAt: aLoadedMethod name) ifNil: [self _addAll: { 'Missing compiled method: ', aLoadedMethod name } to: res for: aLoadedClass name.
"Transcript show: 'Missing ', aLoadedMethod printString"]]
	].
	^res

]
