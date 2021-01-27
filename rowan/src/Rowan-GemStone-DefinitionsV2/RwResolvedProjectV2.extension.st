Extension { #name : 'RwResolvedProjectV2' }

{ #category : '*rowan-gemstone-definitionsv2' }
RwResolvedProjectV2 >> gemstoneDefaultSymbolDictName [
	^ self _loadSpecification gemstoneDefaultSymbolDictName
]

{ #category : '*rowan-gemstone-definitionsv2' }
RwResolvedProjectV2 >> gemstoneDefaultSymbolDictNameForUser: userId [
	^ self _loadSpecification gemstoneDefaultSymbolDictNameForUser: userId
]

{ #category : '*rowan-gemstone-definitionsv2' }
RwResolvedProjectV2 >> gemstoneSetDefaultSymbolDictNameForUser: userId to: symbolDictName [
	self _loadSpecification gemstoneSetDefaultSymbolDictNameForUser: userId to: symbolDictName
]

{ #category : '*rowan-gemstone-definitionsv2' }
RwResolvedProjectV2 >> gemstoneSetDefaultSymbolDictNameTo: symbolDictName [
	self _loadSpecification gemstoneSetDefaultSymbolDictNameTo: symbolDictName
]

{ #category : '*rowan-gemstone-definitionsv2' }
RwResolvedProjectV2 >> gemstoneSetDefaultUseSessionMethodsForExtensionsForUser: userId to: aBool [
	self _loadSpecification
		gemstoneSetDefaultUseSessionMethodsForExtensionsForUser: userId
		to: aBool
]

{ #category : '*rowan-gemstone-definitionsv2' }
RwResolvedProjectV2 >> gemstoneSetDefaultUseSessionMethodsForExtensionsTo: aBool [
	self _loadSpecification
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
	| resolvedLoadComponents |
	resolvedLoadComponents := self _projectComponents.
	^ resolvedLoadComponents
		gemstoneSymbolDictNameForPackageNamed: packageName
		forUser: userId
		ifAbsent: [ 
			"no entry for this package, use the defaultSymbolDictName"
			^ self gemstoneDefaultSymbolDictNameForUser: userId ]
]

{ #category : '*rowan-gemstone-definitionsv2' }
RwResolvedProjectV2 >> gemstoneSymbolDictNameForPackageNamed: packageName forUser: userId ifAbsent: absentBlock [
	^ self _projectComponents
		gemstoneSymbolDictNameForPackageNamed: packageName
		forUser: userId
		ifAbsent: absentBlock
]

{ #category : '*rowan-gemstone-definitionsv2' }
RwResolvedProjectV2 >> methodEnvForPackageNamed: packageName [

	^self _loadSpecification gemstoneDefaultMethodEnvForUser: Rowan image currentUserId
]

{ #category : '*rowan-gemstone-definitionsv2' }
RwResolvedProjectV2 >> useSessionMethodsForExtensionsForPackageNamed: packageName [
	| default |
	default := self _loadSpecification
		gemstoneDefaultUseSessionMethodsForExtensionsForUser:
			Rowan image currentUserId.
	(self componentForPackageNamed: packageName)
		ifNil: [ ^ default ]
		ifNotNil: [ :component | 
			| packageMap propertiesMap |
			packageMap := (component
				conditionalPackageMapSpecsAtGemStoneUserId: Rowan image currentUserId)
				at: #'packageNameToPlatformPropertiesMap'
				ifAbsent: [ 
					(component
						conditionalPackageMapSpecsAtGemStoneUserId: self _gemstoneAllUsersName)
						at: #'packageNameToPlatformPropertiesMap'
						ifAbsent: [ ^ default ] ].
			propertiesMap := packageMap at: packageName ifAbsent: [ ^ default ].
			^ propertiesMap at: 'useSessionMethodsForExtensions' ifAbsent: [ ^ default ] ]
]
