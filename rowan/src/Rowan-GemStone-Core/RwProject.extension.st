Extension { #name : 'RwProject' }

{ #category : '*rowan-gemstone-core' }
RwProject >> defaultSymbolDictName [

	^ self _gemstonePlatformSpec defaultSymbolDictName
]

{ #category : '*rowan-gemstone-core' }
RwProject >> defaultUserId [

	^ self _gemstonePlatformSpec defaultUserId
]

{ #category : '*rowan-gemstone-core' }
RwProject >> defaultUseSessionMethodsForExtensions [

	^ self _gemstonePlatformSpec defaultUseSessionMethodsForExtensions
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
RwProject >> userIdForPackageNamed: packageName [

	^ self _gemstonePlatformSpec userIdForPackageNamed: packageName
]

{ #category : '*rowan-gemstone-core' }
RwProject >> useSessionMethodsForExtensionsForPackageNamed: packageName [

	^ self _gemstonePlatformSpec
		useSessionMethodsForExtensionsForPackageNamed: packageName
]
