Extension { #name : 'RwUnmanagedProjectDefinition' }

{ #category : '*rowan-gemstone-definitions' }
RwUnmanagedProjectDefinition >> defaultSymbolDictName [

	^ 'UnmanagedPackages'
]

{ #category : '*rowan-gemstone-definitions' }
RwUnmanagedProjectDefinition >> methodEnvForPackageNamed: packageName [

	^ 0
]

{ #category : '*rowan-gemstone-definitions' }
RwUnmanagedProjectDefinition >> projectOwnerId [

	^ Rowan image currentUserId
]

{ #category : '*rowan-gemstone-definitions' }
RwUnmanagedProjectDefinition >> symbolDictNameForPackageNamed: packageName [

	^ self defaultSymbolDictName
]

{ #category : '*rowan-gemstone-definitions' }
RwUnmanagedProjectDefinition >> useSessionMethodsForExtensionsForPackageNamed: packageName [

	^ true
]
