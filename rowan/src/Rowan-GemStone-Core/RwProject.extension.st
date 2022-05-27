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
