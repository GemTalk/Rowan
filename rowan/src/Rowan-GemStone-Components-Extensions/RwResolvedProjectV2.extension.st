Extension { #name : 'RwResolvedProjectV2' }

{ #category : '*rowan-gemstone-components-extensions' }
RwResolvedProjectV2 >> gemstoneSetDefaultSymbolDictNameForUser: userId to: symbolDictName [
	self _loadSpecification gemstoneSetDefaultSymbolDictNameForUser: userId to: symbolDictName
]

{ #category : '*rowan-gemstone-components-extensions' }
RwResolvedProjectV2 >> gemstoneSetDefaultSymbolDictNameTo: symbolDictName [
	self _loadSpecification gemstoneSetDefaultSymbolDictNameTo: symbolDictName
]

{ #category : '*rowan-gemstone-components-extensions' }
RwResolvedProjectV2 >> gemstoneSetSymbolDictName: symbolDictName forPackageNamed: packageName [
	self _projectDefinition gemstoneSetSymbolDictName: symbolDictName forPackageNamed: packageName
]
