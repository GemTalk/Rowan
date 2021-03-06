Extension { #name : 'RwProjectDefinitionV2' }

{ #category : '*rowan-gemstone-definitionsv2' }
RwProjectDefinitionV2 >> _gemstoneAllUsersName [
	^ RwLoadSpecificationV2 _gemstoneAllUsersName
]

{ #category : '*rowan-gemstone-definitionsv2' }
RwProjectDefinitionV2 >> gemstoneDefaultSymbolDictNameForUser: userId [
	^ self projectSpecification gemstoneDefaultSymbolDictNameForUser: userId
]

{ #category : '*rowan-gemstone-definitionsv2' }
RwProjectDefinitionV2 >> gemstoneSetSymbolDictName: symbolDictName forPackageNamed: packageName [
	self components
		gemstoneSetSymbolDictName: symbolDictName
		forPackageNamed: packageName
]

{ #category : '*rowan-gemstone-definitionsv2' }
RwProjectDefinitionV2 >> gemstoneSetSymbolDictNameForUser: userId to: symbolDictName forPackageNamed: packageName [
	self components
		gemstoneSetSymbolDictNameForUser: userId
		to: symbolDictName
		forPackageNamed: packageName
]

{ #category : '*rowan-gemstone-definitionsv2' }
RwProjectDefinitionV2 >> gemstoneSetUseSessionMethodsForExtensions: aBool forPackageNamed: packageName [
	self components
		gemstoneSetUseSessionMethodsForExtensions: aBool
		forPackageNamed: packageName
]

{ #category : '*rowan-gemstone-definitionsv2' }
RwProjectDefinitionV2 >> gemstoneSetUseSessionMethodsForExtensionsForUser: userId to: aBool forPackageNamed: packageName [
	self components
		gemstoneSetUseSessionMethodsForExtensionsForUser: userId
		to: aBool
		forPackageNamed: packageName
]

{ #category : '*rowan-gemstone-definitionsv2' }
RwProjectDefinitionV2 >> gemstoneSymbolDictNameForPackageNamed: packageName forUser: userId ifAbsent: absentBlock [
	^ self components
		gemstoneSymbolDictNameForPackageNamed: packageName
		forUser: userId
		ifAbsent: absentBlock
]
