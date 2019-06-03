Extension { #name : 'RwProjectDefinition' }

{ #category : '*rowan-gemstone-definitions' }
RwProjectDefinition >> asLoadedSymbolDictProject [

	^ RwGsLoadedSymbolDictProject newForLoadSpecification: self specification.
]

{ #category : '*rowan-gemstone-definitions' }
RwProjectDefinition >> defaultSymbolDictName [

	^ (self specification platformSpec at: 'gemstone') defaultSymbolDictName
]

{ #category : '*rowan-gemstone-definitions' }
RwProjectDefinition >> defaultSymbolDictName: symbolDictName [

	(self specification platformSpec at: 'gemstone')
		defaultSymbolDictName: symbolDictName
]

{ #category : '*rowan-gemstone-definitions' }
RwProjectDefinition >> defaultUseSessionMethodsForExtensions: aBool [

	(self specification platformSpec at: 'gemstone')
		defaultUseSessionMethodsForExtensions: aBool
]

{ #category : '*rowan-gemstone-definitions' }
RwProjectDefinition >> methodEnvForPackageNamed: packageName [

	^ (self specification platformSpec at: 'gemstone')
		methodEnvForPackageNamed: packageName
]

{ #category : '*rowan-gemstone-definitions' }
RwProjectDefinition >> projectOwnerId [

	^ (self specification platformSpec at: 'gemstone') projectOwnerId
]

{ #category : '*rowan-gemstone-definitions' }
RwProjectDefinition >> projectOwnerId: aUserId [

	(self specification platformSpec at: 'gemstone') projectOwnerId: aUserId
]

{ #category : '*rowan-gemstone-definitions' }
RwProjectDefinition >> setSymbolDictName: symbolDictName forPackageNamed: packageName [

	(self specification platformSpec at: 'gemstone')
		setSymbolDictName: symbolDictName
		forPackageNamed: packageName

]

{ #category : '*rowan-gemstone-definitions' }
RwProjectDefinition >> setUseSessionMethodsForExtensions: aBool forPackageNamed: packageName [

	(self specification platformSpec at: 'gemstone')
		setUseSessionMethodsForExtensions: aBool
		forPackageNamed: packageName

]

{ #category : '*rowan-gemstone-definitions' }
RwProjectDefinition >> symbolDictionaryRegistryClass [

	^ RwGsSymbolDictionaryRegistry
]

{ #category : '*rowan-gemstone-definitions' }
RwProjectDefinition >> symbolDictNameForPackageNamed: packageName [

	^ (self specification platformSpec at: 'gemstone')
		symbolDictNameForPackageNamed: packageName
]

{ #category : '*rowan-gemstone-definitions' }
RwProjectDefinition >> updateGsPlatformSpecLoadedProjectInfo: projectInfo [

	| spec gemstoneSpec thePackageMapSpecs |
	spec := self specification.
	thePackageMapSpecs := projectInfo at:  'packageMapSpecs' .
	gemstoneSpec := spec platformSpec at: 'gemstone'.
	(thePackageMapSpecs at: #defaultSymbolDictName otherwise: nil) 
		ifNotNil: [:name | gemstoneSpec defaultSymbolDictName: name ].
	(thePackageMapSpecs at: #defaultUseSessionMethodsForExtensions otherwise: nil) 
		ifNotNil: [:boolean | 
			gemstoneSpec defaultUseSessionMethodsForExtensions: boolean  ].
	(thePackageMapSpecs at: #packageNameToPlatformPropertiesMap otherwise: nil) 
		ifNotNil: [:map | gemstoneSpec packageNameToPlatformPropertiesMap: map]

]

{ #category : '*rowan-gemstone-definitions' }
RwProjectDefinition >> useSessionMethodsForExtensionsForPackageNamed: packageName [

	^ (self specification platformSpec at: 'gemstone')
		useSessionMethodsForExtensionsForPackageNamed: packageName
]
