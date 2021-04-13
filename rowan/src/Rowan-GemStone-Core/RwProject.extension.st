Extension { #name : 'RwProject' }

{ #category : '*rowan-gemstone-core' }
RwProject >> exportTopazFormatTo: filePath [
	^ self
		logClassCreation: false
		excludeClassInitializers: false
		excludeRemoveAllMethods: false
]

{ #category : '*rowan-gemstone-core' }
RwProject >> exportTopazFormatTo: filePath logClassCreation: logClassCreation excludeClassInitializers: excludeClassInitializers excludeRemoveAllMethods: excludeRemoveAllMethods [
	^ self _loadedProject asDefinition
		exportTopazFormatTo: filePath
		logClassCreation: logClassCreation
		excludeClassInitializers: excludeClassInitializers
		excludeRemoveAllMethods: excludeRemoveAllMethods
]

{ #category : '*rowan-gemstone-core' }
RwProject >> exportTopazFormatTo: filePath logClassCreation: logClassCreation excludeClassInitializers: excludeClassInitializers excludeRemoveAllMethods: excludeRemoveAllMethods usingPackageNamesMap: packageNamesMap [
	^ self _loadedProject asDefinition
		exportTopazFormatTo: filePath
		logClassCreation: logClassCreation
		excludeClassInitializers: excludeClassInitializers
		excludeRemoveAllMethods: excludeRemoveAllMethods
		usingPackageNamesMap: packageNamesMap
]

{ #category : '*rowan-gemstone-core' }
RwProject >> gemstoneDefaultSymbolDictName [
	^ self _loadedProject gemstoneDefaultSymbolDictName
]

{ #category : '*rowan-gemstone-core' }
RwProject >> methodEnvForPackageNamed: packageName [

	^ self _gemstonePlatformSpec methodEnvForPackageNamed: packageName
]

{ #category : '*rowan-gemstone-core' }
RwProject >> symbolDictNameForPackageNamed: packageName [

	^ self _loadedProject symbolDictNameForPackageNamed: packageName
]

{ #category : '*rowan-gemstone-core' }
RwProject >> useSessionMethodsForExtensionsForPackageNamed: packageName [

	^ self _gemstonePlatformSpec
		useSessionMethodsForExtensionsForPackageNamed: packageName
]
