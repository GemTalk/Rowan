Class {
	#name : 'RwGsMethodPropertiesSymDictPatchV2',
	#superclass : 'RwGsMethodPatchV2',
	#category : 'Rowan-GemStone-LoaderV2'
}

{ #category : 'installing' }
RwGsMethodPropertiesSymDictPatchV2 >> installPropertiesPatchNewClasses: createdClasses andExistingClasses: tempSymbols [

	" update method protocol and update loadedMethod with new compiled method"

	| methodDictionary oldCompiledMethod |
	self primeBehaviorNewClasses: createdClasses andExistingClasses: tempSymbols.
	behavior
		ifNil: [ self error: 'Class ' , self className printString , ' not found.' ].

	methodDictionary := (behavior persistentMethodDictForEnv: 0 ) ifNil:[ Dictionary new ].
	selector := methodDefinition selector.
	oldCompiledMethod := methodDictionary
		at: selector
		ifAbsent: [ 
			self
				error:
					'Internal error -- no existing CompileMethod found for patched method.' ].

	self symbolDictionaryRegistry
		moveCompiledMethod: oldCompiledMethod
		toProtocol: self propertiesProtocolName
		implementationClass: RwGsSymbolDictionaryRegistry_Implementation

]

{ #category : 'initializers' }
RwGsMethodPropertiesSymDictPatchV2 >> isAnInitializer [

	"answer true if I represent a class initialization method"

	^ false

]

{ #category : 'initializers' }
RwGsMethodPropertiesSymDictPatchV2 >> runInitializer [
	"noop"
]
