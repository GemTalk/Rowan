Extension { #name : 'RwProjectDefinition' }

{ #category : '*rowan-gemstone-core' }
RwProjectDefinition >> defaultSymbolDictName [

	^ (self specification platformSpec at: 'gemstone') defaultSymbolDictName
]

{ #category : '*rowan-gemstone-core' }
RwProjectDefinition >> defaultSymbolDictName: symbolDictName [

	(self specification platformSpec at: 'gemstone')
		defaultSymbolDictName: symbolDictName
]

{ #category : '*rowan-gemstone-core' }
RwProjectDefinition >> defaultUseSessionMethodsForExtensions: aBool [

	(self specification platformSpec at: 'gemstone')
		defaultUseSessionMethodsForExtensions: aBool
]

{ #category : '*rowan-gemstone-core' }
RwProjectDefinition >> methodEnvForPackageNamed: packageName [

	^ (self specification platformSpec at: 'gemstone')
		methodEnvForPackageNamed: packageName
]

{ #category : '*rowan-gemstone-core' }
RwProjectDefinition >> projectOwnerId [

	^ (self specification platformSpec at: 'gemstone') projectOwnerId
]

{ #category : '*rowan-gemstone-core' }
RwProjectDefinition >> projectOwnerId: aUserId [

	(self specification platformSpec at: 'gemstone') projectOwnerId: aUserId
]

{ #category : '*rowan-gemstone-core' }
RwProjectDefinition >> symbolDictionaryRegistryClass [

	^ RwGsSymbolDictionaryRegistry
]

{ #category : '*rowan-gemstone-core' }
RwProjectDefinition >> symbolDictName: symbolDictName forPackageNamed: packageName [

	(self specification platformSpec at: 'gemstone')
		symbolDictName: symbolDictName
		forPackageNamed: packageName
]

{ #category : '*rowan-gemstone-core' }
RwProjectDefinition >> symbolDictNameForPackageNamed: packageName [

	^ (self specification platformSpec at: 'gemstone')
		symbolDictNameForPackageNamed: packageName
]

{ #category : '*rowan-gemstone-core' }
RwProjectDefinition >> useSessionMethodsForExtensions: aBool forPackageNamed: packageName [

	(self specification platformSpec at: 'gemstone')
		useSessionMethodsForExtensions: aBool
		forPackageNamed: packageName
]

{ #category : '*rowan-gemstone-core' }
RwProjectDefinition >> useSessionMethodsForExtensionsForPackageNamed: packageName [

	^ (self specification platformSpec at: 'gemstone')
		useSessionMethodsForExtensionsForPackageNamed: packageName
]
