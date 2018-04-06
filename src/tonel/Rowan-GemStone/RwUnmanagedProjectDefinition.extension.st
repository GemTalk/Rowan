Extension { #name : 'RwUnmanagedProjectDefinition' }

{ #category : '*rowan-gemstone' }
RwUnmanagedProjectDefinition >> defaultSymbolDictName [

	^ 'UnmanagedPackages'
]

{ #category : '*rowan-gemstone' }
RwUnmanagedProjectDefinition >> methodEnvForPackageNamed: packageName [

	^ 0
]

{ #category : '*rowan-gemstone' }
RwUnmanagedProjectDefinition >> projectOwnerId [

	^ Rowan image currentUserId
]

{ #category : '*rowan-gemstone' }
RwUnmanagedProjectDefinition >> symbolDictNameForPackageNamed: packageName [

	^ self defaultSymbolDictName
]

{ #category : '*rowan-gemstone' }
RwUnmanagedProjectDefinition >> useSessionMethodsForExtensionsForPackageNamed: packageName [

	^ true
]
