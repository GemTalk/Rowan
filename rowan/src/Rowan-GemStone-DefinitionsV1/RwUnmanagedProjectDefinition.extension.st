Extension { #name : 'RwUnmanagedProjectDefinition' }

{ #category : '*rowan-gemstone-definitionsv1' }
RwUnmanagedProjectDefinition >> defaultSymbolDictName [

	^ 'UnmanagedPackages'
]

{ #category : '*rowan-gemstone-definitionsv1' }
RwUnmanagedProjectDefinition >> methodEnvForPackageNamed: packageName [

	^ 0
]

{ #category : '*rowan-gemstone-definitionsv1' }
RwUnmanagedProjectDefinition >> projectOwnerId [

	^ Rowan image currentUserId
]

{ #category : '*rowan-gemstone-definitionsv1' }
RwUnmanagedProjectDefinition >> symbolDictNameForPackageNamed: packageName [

	^ self defaultSymbolDictName
]

{ #category : '*rowan-gemstone-definitionsv1' }
RwUnmanagedProjectDefinition >> useSessionMethodsForExtensionsForPackageNamed: packageName [

	^ true
]
