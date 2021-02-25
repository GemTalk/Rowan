Extension { #name : 'RwPackage' }

{ #category : '*rowan-gemstone-core' }
RwPackage >> methodEnv [

	^ self _gemstonePlatformSpec methodEnvForPackageNamed: self name
]
