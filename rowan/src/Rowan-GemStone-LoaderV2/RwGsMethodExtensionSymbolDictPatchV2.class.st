Class {
	#name : 'RwGsMethodExtensionSymbolDictPatchV2',
	#superclass : 'RwGsMethodPatchV2',
	#category : 'Rowan-GemStone-LoaderV2'
}

{ #category : 'installing' }
RwGsMethodExtensionSymbolDictPatchV2 >> installMethod [

	self symbolDictionaryRegistry
		addExtensionCompiledMethod: compiledMethod
		for: behavior
		protocol: self propertiesProtocolName
		toPackageNamed: self packageName
		 implementationClass: RwGsSymbolDictionaryRegistry_ImplementationV2.
	selector := compiledMethod selector
]
