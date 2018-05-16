"
No class-specific documentation for CypGsMethodAdditionPatch, hierarchy is: 
Object
  CypGsPatch
    CypGsMethodAdditionPatch( isMeta methodDefinition classDefinition packageDefinition)

"
Class {
	#name : 'RwGsMethodAdditionSymbolDictPatch',
	#superclass : 'RwGsMethodPatch',
	#category : 'Rowan-GemStone-Loader'
}

{ #category : 'installing' }
RwGsMethodAdditionSymbolDictPatch >> installMethod [

	self symbolDictionaryRegistry
		addNewCompiledMethod: compiledMethod
		for: behavior
		protocol: self propertiesProtocolName
		toPackageNamed: self packageName.
	selector := compiledMethod selector
]

{ #category : 'installing' }
RwGsMethodAdditionSymbolDictPatch >> installMovedDeletedMethod [

	self symbolDictionaryRegistry
		addMovedDeletedMethod: compiledMethod
		for: behavior
		protocol: self propertiesProtocolName
		toPackageNamed: self packageName.
	selector := compiledMethod selector

]
