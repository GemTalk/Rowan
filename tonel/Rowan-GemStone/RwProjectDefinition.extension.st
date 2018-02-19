Extension { #name : 'RwProjectDefinition' }

{ #category : '*rowan-gemstone' }
RwProjectDefinition >> defaultSymbolDictName [

	^ (self specification platformSpec at: 'gemstone') defaultSymbolDictName
]

{ #category : '*rowan-gemstone' }
RwProjectDefinition >> defaultSymbolDictName: symbolDictName [

	(self specification platformSpec at: 'gemstone')
		defaultSymbolDictName: symbolDictName
]

{ #category : '*rowan-gemstone' }
RwProjectDefinition >> methodEnvForPackageNamed: packageName [

	^ (self specification platformSpec at: 'gemstone')
		methodEnvForPackageNamed: packageName
]

{ #category : '*rowan-gemstone' }
RwProjectDefinition >> packageSymbolDictionaryClass [

	^ RwGsPackageSymbolDictionary
]

{ #category : '*rowan-gemstone' }
RwProjectDefinition >> projectOwnerId [

	^ (self specification platformSpec at: 'gemstone') projectOwnerId
]

{ #category : '*rowan-gemstone' }
RwProjectDefinition >> projectOwnerId: aUserId [

	(self specification platformSpec at: 'gemstone') projectOwnerId: aUserId
]

{ #category : '*rowan-gemstone' }
RwProjectDefinition >> symbolDictName: symbolDictName forPackageNamed: packageName [

	(self specification platformSpec at: 'gemstone')
		symbolDictName: symbolDictName
		forPackageNamed: packageName
]

{ #category : '*rowan-gemstone' }
RwProjectDefinition >> symbolDictNameForPackageNamed: packageName [

	^ (self specification platformSpec at: 'gemstone')
		symbolDictNameForPackageNamed: packageName
]

{ #category : '*rowan-gemstone' }
RwProjectDefinition >> useSessionMethodsForExtensionsForPackageNamed: packageName [

	^ (self specification platformSpec at: 'gemstone')
		useSessionMethodsForExtensionsForPackageNamed: packageName
]
