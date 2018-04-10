"
No class-specific documentation for CypGsMethodAdditionPatch, hierarchy is: 
Object
  CypGsPatch
    CypGsMethodAdditionPatch( isMeta methodDefinition classDefinition packageDefinition)

"
Class {
	#name : 'RwGsMethodExtensionSymbolDictPatch',
	#superclass : 'RwGsMethodPatch',
	#category : 'Rowan-GemStone'
}

{ #category : 'installing' }
RwGsMethodExtensionSymbolDictPatch >> installMethod [

	self symbolDictionaryRegistry
		addExtensionCompiledMethod: compiledMethod
		for: behavior
		protocol: self propertiesProtocolName
		toPackageNamed: self packageName.
	selector := compiledMethod selector
]
