Extension { #name : 'RwProject' }

{ #category : '*rowan-gemstone-core' }
RwProject >> defaultSymbolDictName [

	^ self _gemstonePlatformSpec defaultSymbolDictName
]

{ #category : '*rowan-gemstone-core' }
RwProject >> defaultSymbolDictName: aString [

	^ self _loadedProject defaultSymbolDictName: aString
]

{ #category : '*rowan-gemstone-core' }
RwProject >> defaultUseSessionMethodsForExtensions [

	^ self _gemstonePlatformSpec defaultUseSessionMethodsForExtensions
]

{ #category : '*rowan-gemstone-core' }
RwProject >> exportTopazFormatTo: filePath [


	^ self _loadedProject asDefinition exportTopazFormatTo: filePath
]

{ #category : '*rowan-gemstone-core' }
RwProject >> exportTopazFormatTo: filePath usingPackageNamesMap: packageNamesMap [
	^ self _loadedProject asDefinition
		exportTopazFormatTo: filePath
		usingPackageNamesMap: packageNamesMap
]

{ #category : '*rowan-gemstone-core' }
RwProject >> methodEnvForPackageNamed: packageName [

	^ self _gemstonePlatformSpec methodEnvForPackageNamed: packageName
]

{ #category : '*rowan-gemstone-core' }
RwProject >> symbolDictNameForPackageNamed: packageName [

	^ self _gemstonePlatformSpec symbolDictNameForPackageNamed: packageName
]

{ #category : '*rowan-gemstone-core' }
RwProject >> useSessionMethodsForExtensionsForPackageNamed: packageName [

	^ self _gemstonePlatformSpec
		useSessionMethodsForExtensionsForPackageNamed: packageName
]
