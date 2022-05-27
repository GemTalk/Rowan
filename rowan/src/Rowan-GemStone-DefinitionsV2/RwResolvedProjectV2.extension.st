Extension { #name : 'RwResolvedProjectV2' }

{ #category : '*rowan-gemstone-definitionsv2' }
RwResolvedProjectV2 >> gemstoneDefaultSymbolDictName [
	^ self loadSpecification gemstoneDefaultSymbolDictName
]

{ #category : '*rowan-gemstone-definitionsv2' }
RwResolvedProjectV2 >> gemstoneDefaultSymbolDictNameForUser: userId [
	^ self loadSpecification gemstoneDefaultSymbolDictNameForUser: userId
]

{ #category : '*rowan-gemstone-definitionsv2' }
RwResolvedProjectV2 >> gemstoneDefaultUseSessionMethodsForExtensions [
	^ self loadSpecification gemstoneDefaultUseSessionMethodsForExtensions
]

{ #category : '*rowan-gemstone-definitionsv2' }
RwResolvedProjectV2 >> gemstoneDefaultUseSessionMethodsForExtensionsForUser: userId [
	^ self loadSpecification
		gemstoneDefaultUseSessionMethodsForExtensionsForUser:
			Rowan image currentUserId
]

{ #category : '*rowan-gemstone-definitionsv2' }
RwResolvedProjectV2 >> gemstoneSetDefaultSymbolDictNameForUser: userId to: symbolDictName [
	self loadSpecification gemstoneSetDefaultSymbolDictNameForUser: userId to: symbolDictName
]

{ #category : '*rowan-gemstone-definitionsv2' }
RwResolvedProjectV2 >> gemstoneSetDefaultSymbolDictNameTo: symbolDictName [
	self loadSpecification gemstoneSetDefaultSymbolDictNameTo: symbolDictName
]

{ #category : '*rowan-gemstone-definitionsv2' }
RwResolvedProjectV2 >> gemstoneSetDefaultUseSessionMethodsForExtensionsForUser: userId to: aBool [
	self loadSpecification
		gemstoneSetDefaultUseSessionMethodsForExtensionsForUser: userId
		to: aBool
]

{ #category : '*rowan-gemstone-definitionsv2' }
RwResolvedProjectV2 >> gemstoneSetDefaultUseSessionMethodsForExtensionsTo: aBool [
	self loadSpecification
		gemstoneSetDefaultUseSessionMethodsForExtensionsTo: aBool
]

{ #category : '*rowan-gemstone-definitionsv2' }
RwResolvedProjectV2 >> gemstoneSetSymbolDictName: symbolDictName forPackageNamed: packageName [
	self _projectComponents gemstoneSetSymbolDictName: symbolDictName forPackageNamed: packageName
]

{ #category : '*rowan-gemstone-definitionsv2' }
RwResolvedProjectV2 >> gemstoneSetSymbolDictNameForUser: userId to: symbolDictName forPackageNamed: packageName [
	self _projectComponents
		gemstoneSetSymbolDictNameForUser: userId
		to: symbolDictName
		forPackageNamed: packageName
]

{ #category : '*rowan-gemstone-definitionsv2' }
RwResolvedProjectV2 >> gemstoneSetUseSessionMethodsForExtensions: aBool forPackageNamed: packageName [
	self _projectComponents
		gemstoneSetUseSessionMethodsForExtensions: aBool
		forPackageNamed: packageName
]

{ #category : '*rowan-gemstone-definitionsv2' }
RwResolvedProjectV2 >> gemstoneSetUseSessionMethodsForExtensionsForUser: userId to: aBool forPackageNamed: packageName [
	self _projectComponents
		gemstoneSetUseSessionMethodsForExtensionsForUser: userId
		to: aBool
		forPackageNamed: packageName
]

{ #category : '*rowan-gemstone-definitionsv2' }
RwResolvedProjectV2 >> gemstoneSymbolDictNameForPackageNamed: packageName [
	^ self
		gemstoneSymbolDictNameForPackageNamed: packageName
		forUser: Rowan image currentUserId
]

{ #category : '*rowan-gemstone-definitionsv2' }
RwResolvedProjectV2 >> gemstoneSymbolDictNameForPackageNamed: packageName forUser: userId [
	| resolvedLoadComponents packagePropertiesMap |
	resolvedLoadComponents := self _projectComponents.
	packagePropertiesMap := resolvedLoadComponents
		gemstonePackagePropertyPropertyMapForPackageNamed: packageName
		forUser: userId
		ifAbsent: [ ^ self gemstoneDefaultSymbolDictNameForUser: userId ].
	^ (packagePropertiesMap
		at: packageName
		ifAbsent: [ ^ self gemstoneDefaultSymbolDictNameForUser: userId ])
		at: 'symbolDictName'
		ifAbsent: [ ^ self gemstoneDefaultSymbolDictNameForUser: userId ]
]

{ #category : '*rowan-gemstone-definitionsv2' }
RwResolvedProjectV2 >> gemstoneUseSessionMethodsForExtensionsForPackageNamed: packageName [
	^ self
		gemstoneUseSessionMethodsForExtensionsForPackageNamed: packageName
		forUser: Rowan image currentUserId
]

{ #category : '*rowan-gemstone-definitionsv2' }
RwResolvedProjectV2 >> gemstoneUseSessionMethodsForExtensionsForPackageNamed: packageName forUser: userId [
	| resolvedLoadComponents packagePropertiesMap |
	resolvedLoadComponents := self _projectComponents.
	packagePropertiesMap := resolvedLoadComponents
		gemstonePackagePropertyPropertyMapForPackageNamed: packageName
		forUser: userId
		ifAbsent: [ ^ self gemstoneDefaultUseSessionMethodsForExtensionsForUser: userId ].
	^ (packagePropertiesMap
		at: packageName
		ifAbsent: [ ^ self gemstoneDefaultUseSessionMethodsForExtensionsForUser: userId ])
		at: 'useSessionMethodsForExtensions'
		ifAbsent: [ ^ self gemstoneDefaultUseSessionMethodsForExtensionsForUser: userId ]
]

{ #category : '*rowan-gemstone-definitionsv2' }
RwResolvedProjectV2 >> methodEnvForPackageNamed: packageName [

	^self loadSpecification gemstoneDefaultMethodEnvForUser: Rowan image currentUserId
]
