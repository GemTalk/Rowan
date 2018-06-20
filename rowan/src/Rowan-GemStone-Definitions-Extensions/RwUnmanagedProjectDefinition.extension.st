Extension { #name : 'RwUnmanagedProjectDefinition' }

{ #category : 'rowan-gemstone-definitions-extensions' }
RwUnmanagedProjectDefinition >> defaultSymbolDictName [

	^ 'UnmanagedPackages'
]

{ #category : 'rowan-gemstone-definitions-extensions' }
RwUnmanagedProjectDefinition >> methodEnvForPackageNamed: packageName [

	^ 0
]

{ #category : 'rowan-gemstone-definitions-extensions' }
RwUnmanagedProjectDefinition >> projectOwnerId [

	^ Rowan image currentUserId
]

{ #category : 'rowan-gemstone-definitions-extensions' }
RwUnmanagedProjectDefinition >> symbolDictNameForPackageNamed: packageName [

	^ self defaultSymbolDictName
]

{ #category : 'rowan-gemstone-definitions-extensions' }
RwUnmanagedProjectDefinition >> useSessionMethodsForExtensionsForPackageNamed: packageName [

	^ true
]
