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
	#category : 'Rowan-GemStone'
}

{ #category : 'installing' }
RwGsMethodPropertiesSymDictPatch >> installPropertiesPatchNewClasses: createdClasses andExistingClasses: tempSymbols [

	" update method protocol and update loadedMethod with new compiled method"

	| methodDictionary oldCompiledMethod |
	self primeBehaviorNewClasses: createdClasses andExistingClasses: tempSymbols.

	methodDictionary := behavior persistentMethodDictForEnv: 0.
	selector := methodDefinition selector.
	oldCompiledMethod := methodDictionary
		at: selector
		ifAbsent: [ 
			self
				error:
					'Internal error -- no existing CompileMethod found for patched method.' ].

	self packageSymbolDictionary
		moveCompiledMethod: oldCompiledMethod
		toProtocol: self propertiesProtocolName
]

{ #category : 'initializers' }
RwGsMethodPropertiesSymDictPatch >> runInitializer [
	"noop"
]
