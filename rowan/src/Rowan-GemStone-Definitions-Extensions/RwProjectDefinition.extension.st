Extension { #name : 'RwProjectDefinition' }

{ #category : '*rowan-gemstone-definitions-extensions' }
RwProjectDefinition >> defaultSymbolDictName [

	^ (self specification platformSpec at: 'gemstone') defaultSymbolDictName
]

{ #category : '*rowan-gemstone-definitions-extensions' }
RwProjectDefinition >> defaultSymbolDictName: symbolDictName [

	(self specification platformSpec at: 'gemstone')
		defaultSymbolDictName: symbolDictName
]

{ #category : '*rowan-gemstone-definitions-extensions' }
RwProjectDefinition >> defaultUseSessionMethodsForExtensions: aBool [

	(self specification platformSpec at: 'gemstone')
		defaultUseSessionMethodsForExtensions: aBool
]

{ #category : '*rowan-gemstone-definitions-extensions' }
RwProjectDefinition >> methodEnvForPackageNamed: packageName [

	^ (self specification platformSpec at: 'gemstone')
		methodEnvForPackageNamed: packageName
]

{ #category : '*rowan-gemstone-definitions-extensions' }
RwProjectDefinition >> projectOwnerId [

	^ (self specification platformSpec at: 'gemstone') projectOwnerId
]

{ #category : '*rowan-gemstone-definitions-extensions' }
RwProjectDefinition >> projectOwnerId: aUserId [

	(self specification platformSpec at: 'gemstone') projectOwnerId: aUserId
]

{ #category : '*rowan-gemstone-definitions-extensions' }
RwProjectDefinition >> setSymbolDictName: symbolDictName forPackageNamed: packageName [

	(self specification platformSpec at: 'gemstone')
		setSymbolDictName: symbolDictName
		forPackageNamed: packageName

]

{ #category : '*rowan-gemstone-definitions-extensions' }
RwProjectDefinition >> setUseSessionMethodsForExtensions: aBool forPackageNamed: packageName [

	(self specification platformSpec at: 'gemstone')
		setUseSessionMethodsForExtensions: aBool
		forPackageNamed: packageName

]

{ #category : '*rowan-gemstone-definitions-extensions' }
RwProjectDefinition >> symbolDictionaryRegistryClass [

	^ RwGsSymbolDictionaryRegistry
]

{ #category : '*rowan-gemstone-definitions-extensions' }
RwProjectDefinition >> symbolDictNameForPackageNamed: packageName [

	^ (self specification platformSpec at: 'gemstone')
		symbolDictNameForPackageNamed: packageName
]

{ #category : '*rowan-gemstone-definitions-extensions' }
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

{ #category : '*rowan-gemstone-definitions-extensions' }
RwProjectDefinition >> useSessionMethodsForExtensionsForPackageNamed: packageName [

	^ (self specification platformSpec at: 'gemstone')
		useSessionMethodsForExtensionsForPackageNamed: packageName
]
