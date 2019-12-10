Extension { #name : 'RwResolvedProjectV2' }

{ #category : '*rowan-gemstone-components-extensions' }
RwResolvedProjectV2 >> asLoadedSymbolDictProject [
	^ RwGsLoadedSymbolDictResolvedProjectV2 newForResolvedProject: self
]

{ #category : '*rowan-gemstone-components-extensions' }
RwResolvedProjectV2 >> gemstoneDefaultSymbolDictNameForUser: userId [
	^ self _loadSpecification gemstoneDefaultSymbolDictNameForUser: userId
]

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

{ #category : '*rowan-gemstone-components-extensions' }
RwResolvedProjectV2 >> gemstoneSymbolDictNameForPackageNamed: packageName [
	^ self
		gemstoneSymbolDictNameForPackageNamed: packageName
		forUser: Rowan image currentUserId
]

{ #category : '*rowan-gemstone-components-extensions' }
RwResolvedProjectV2 >> gemstoneSymbolDictNameForPackageNamed: packageName forUser: userId [
	^ self _projectDefinition
		gemstoneSymbolDictNameForPackageNamed: packageName
		forUser: userId
		ifAbsent: [ 
			"no entry for this package, use the defaultSymbolDictName"
			^ self gemstoneDefaultSymbolDictNameForUser: userId ]
]

{ #category : '*rowan-gemstone-components-extensions' }
RwResolvedProjectV2 >> methodEnvForPackageNamed: packageName [

	^self _loadSpecification gemstoneDefaultMethodEnvForUser: Rowan image currentUserId
]

{ #category : '*rowan-gemstone-components-extensions' }
RwResolvedProjectV2 >> useSessionMethodsForExtensionsForPackageNamed: packageName [
	"Ignore the package name for now ... will do proper package name lookup when we're ready to support method environments"

	"https://github.com/GemTalk/Rowan/issues/533"

	^ self _loadSpecification
		gemstoneDefaultUseSessionMethodsForExtensionsForUser:
			Rowan image currentUserId
]
