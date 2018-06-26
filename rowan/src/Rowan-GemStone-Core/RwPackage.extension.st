Extension { #name : 'RwPackage' }

{ #category : '*rowan-gemstone-core' }
RwPackage >> methodEnv [

	^ self _gemstonePlatformSpec methodEnvForPackageNamed: self name
]

{ #category : '*rowan-gemstone-core' }
RwPackage >> symbolDictName [

	^ self _gemstonePlatformSpec symbolDictNameForPackageNamed: self name
]

{ #category : '*rowan-gemstone-core' }
RwPackage >> useSessionMethodsForExtensions [

	^ self _gemstonePlatformSpec
		useSessionMethodsForExtensionsForPackageNamed: self name
]
