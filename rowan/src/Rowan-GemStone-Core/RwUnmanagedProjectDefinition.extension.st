Extension { #name : 'RwUnmanagedProjectDefinition' }

{ #category : '*rowan-gemstone-core' }
RwUnmanagedProjectDefinition >> defaultSymbolDictName [

	^ 'UnmanagedPackages'
]

{ #category : '*rowan-gemstone-core' }
RwUnmanagedProjectDefinition >> methodEnvForPackageNamed: packageName [

	^ 0
]

{ #category : '*rowan-gemstone-core' }
RwUnmanagedProjectDefinition >> projectOwnerId [

	^ Rowan image currentUserId
]

{ #category : '*rowan-gemstone-core' }
RwUnmanagedProjectDefinition >> symbolDictNameForPackageNamed: packageName [

	^ self defaultSymbolDictName
]

{ #category : '*rowan-gemstone-core' }
RwUnmanagedProjectDefinition >> useSessionMethodsForExtensionsForPackageNamed: packageName [

	^ true
]
