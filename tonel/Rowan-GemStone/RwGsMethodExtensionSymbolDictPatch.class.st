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

{ #category : 'compiling' }
RwGsMethodExtensionSymbolDictPatch >> compileUsingNewClasses: createdClasses andExistingClasses: tempSymbols [

	((projectLoadSpecification platformSpec at: 'gemstone')
		useSessionMethodsForExtensionsForPackageNamed: self packageName)
		ifTrue: [ self error: 'not yet implemented' ]
		ifFalse: [ super compileUsingNewClasses: createdClasses andExistingClasses: tempSymbols ]
]

{ #category : 'installing' }
RwGsMethodExtensionSymbolDictPatch >> installMethod [

	((projectLoadSpecification platformSpec at: 'gemstone')
		useSessionMethodsForExtensionsForPackageNamed: self packageName)
		ifTrue: [ self error: 'not yet implemented' ]
		ifFalse: [ 
			self packageSymbolDictionary
				addExtensionCompiledMethod: compiledMethod
				for: behavior
				protocol: self propertiesProtocolName
				toPackageNamed: self packageName ].
	selector := compiledMethod selector
]
