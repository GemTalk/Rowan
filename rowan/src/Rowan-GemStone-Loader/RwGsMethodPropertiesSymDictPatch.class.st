"
No class-specific documentation for CypGsMethodPropertiesPatch, hierarchy is: 
Object
  CypGsPatch
    CypGsMethodPatch( isMeta methodDefinition classDefinition packageDefinition)
      CypGsMethodPropertiesPatch( propertiesModification)

"
Class {
	#name : 'RwGsMethodPropertiesSymDictPatch',
	#superclass : 'RwGsMethodPatch',
	#category : 'Rowan-GemStone-Loader'
}

{ #category : 'installing' }
RwGsMethodPropertiesSymDictPatch >> installPropertiesPatchNewClasses: createdClasses andExistingClasses: tempSymbols [

	" update method protocol and update loadedMethod with new compiled method"

	| methodDictionary oldCompiledMethod |
	self primeBehaviorNewClasses: createdClasses andExistingClasses: tempSymbols.
	behavior
		ifNil: [ self error: 'Class ' , self className printString , ' not found.' ].

	methodDictionary := behavior rwPersistentMethodDictForEnv: 0.
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
RwGsMethodPropertiesSymDictPatch >> isAnInitializer [

	"answer true if I represent a class initialization method"

	^ false

]

{ #category : 'initializers' }
RwGsMethodPropertiesSymDictPatch >> runInitializer [
	"noop"
]
