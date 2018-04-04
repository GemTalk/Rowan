Extension { #name : 'RwPackage' }

{ #category : '*rowan-gemstone' }
RwPackage >> methodEnv [

	^ self _gemstonePlatformSpec methodEnvForPackageNamed: self name
]

{ #category : '*rowan-gemstone' }
RwPackage >> symbolDictName [

	^ self _gemstonePlatformSpec symbolDictNameForPackageNamed: self name
]

{ #category : '*rowan-gemstone' }
RwPackage >> userId [

	^ self _gemstonePlatformSpec userIdForPackageNamed: self name
]

{ #category : '*rowan-gemstone' }
RwPackage >> useSessionMethodsForExtensions [

	^ self _gemstonePlatformSpec
		useSessionMethodsForExtensionsForPackageNamed: self name
]
