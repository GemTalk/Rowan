Class {
	#name : 'RwGsMethodSourcePatch',
	#superclass : 'RwGsMethodPatch',
	#category : 'Rowan-GemStone'
}

{ #category : 'installing' }
RwGsMethodSourcePatch >> installSourcePatch [
	" update methodDictionary and update loadedMethod with new compiled method"

	| methodDictionary oldCompiledMethod loadedMethod |
	methodDictionary := behavior persistentMethodDictForEnv: 0.
	selector := compiledMethod selector.
	oldCompiledMethod := methodDictionary 
		at: selector 
		ifAbsent: [ self error: 'Internal error -- no existing CompileMethod found for patched method.' ].
	methodDictionary at: selector put: compiledMethod.
	loadedMethod := RwGsLoadedMethod existingForMethod: oldCompiledMethod
				ifAbsent: [
					self
						error: 'Internal error -- no existing LoadedMethod found for patched method.'].
	loadedMethod registerWithNewHandle: compiledMethod

]
