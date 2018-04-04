Extension { #name : 'RwProject' }

{ #category : '*rowan-gemstone' }
RwProject >> defaultSymbolDictName [

	^ self _gemstonePlatformSpec defaultSymbolDictName
]

{ #category : '*rowan-gemstone' }
RwProject >> defaultUserId [

	^ self _gemstonePlatformSpec defaultUserId
]

{ #category : '*rowan-gemstone' }
RwProject >> defaultUseSessionMethodsForExtensions [

	^ self _gemstonePlatformSpec defaultUseSessionMethodsForExtensions
]

{ #category : '*rowan-gemstone' }
RwProject >> methodEnvForPackageNamed: packageName [

	^ self _gemstonePlatformSpec methodEnvForPackageNamed: packageName
]

{ #category : '*rowan-gemstone' }
RwProject >> symbolDictNameForPackageNamed: packageName [

	^ self _gemstonePlatformSpec symbolDictNameForPackageNamed: packageName
]

{ #category : '*rowan-gemstone' }
RwProject >> userIdForPackageNamed: packageName [

	^ self _gemstonePlatformSpec userIdForPackageNamed: packageName
]

{ #category : '*rowan-gemstone' }
RwProject >> useSessionMethodsForExtensionsForPackageNamed: packageName [

	^ self _gemstonePlatformSpec
		useSessionMethodsForExtensionsForPackageNamed: packageName
]
