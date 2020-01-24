Extension { #name : 'RwProjectDefinition' }

{ #category : '*rowan-gemstone-definitionsv1' }
RwProjectDefinition >> asLoadedSymbolDictProject [

	^ RwGsLoadedSymbolDictProject newForLoadSpecification: self specification.
]

{ #category : '*rowan-gemstone-definitionsv1' }
RwProjectDefinition >> defaultSymbolDictName [

	^ (self specification platformSpec at: 'gemstone') defaultSymbolDictName
]

{ #category : '*rowan-gemstone-definitionsv1' }
RwProjectDefinition >> defaultSymbolDictName: symbolDictName [

	(self specification platformSpec at: 'gemstone')
		defaultSymbolDictName: symbolDictName
]

{ #category : '*rowan-gemstone-definitionsv1' }
RwProjectDefinition >> defaultUseSessionMethodsForExtensions: aBool [

	(self specification platformSpec at: 'gemstone')
		defaultUseSessionMethodsForExtensions: aBool
]

{ #category : '*rowan-gemstone-definitionsv1' }
RwProjectDefinition >> methodEnvForPackageNamed: packageName [

	^ (self specification platformSpec at: 'gemstone')
		methodEnvForPackageNamed: packageName
]

{ #category : '*rowan-gemstone-definitionsv1' }
RwProjectDefinition >> projectOwnerId [

	^ (self specification platformSpec at: 'gemstone') projectOwnerId
]

{ #category : '*rowan-gemstone-definitionsv1' }
RwProjectDefinition >> projectOwnerId: aUserId [

	(self specification platformSpec at: 'gemstone') projectOwnerId: aUserId
]

{ #category : '*rowan-gemstone-definitionsv1' }
RwProjectDefinition >> register [

	self specification register
]

{ #category : '*rowan-gemstone-definitionsv1' }
RwProjectDefinition >> setSymbolDictName: symbolDictName forPackageNamed: packageName [

	self projectDefinitionSourceProperty: nil.	"when project definition is loaded,each package needs to update it's target symbol dictionary"
	(self specification platformSpec at: 'gemstone')
		setSymbolDictName: symbolDictName
		forPackageNamed: packageName
]

{ #category : '*rowan-gemstone-definitionsv1' }
RwProjectDefinition >> setUseSessionMethodsForExtensions: aBool forPackageNamed: packageName [

	(self specification platformSpec at: 'gemstone')
		setUseSessionMethodsForExtensions: aBool
		forPackageNamed: packageName

]

{ #category : '*rowan-gemstone-definitionsv1' }
RwProjectDefinition >> symbolDictionaryRegistryClass [

	^ RwGsSymbolDictionaryRegistry
]

{ #category : '*rowan-gemstone-definitionsv1' }
RwProjectDefinition >> symbolDictNameForPackageNamed: packageName [

	^ (self specification platformSpec at: 'gemstone')
		symbolDictNameForPackageNamed: packageName
]

{ #category : '*rowan-gemstone-definitionsv1' }
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

{ #category : '*rowan-gemstone-definitionsv1' }
RwProjectDefinition >> useSessionMethodsForExtensionsForPackageNamed: packageName [

	^ (self specification platformSpec at: 'gemstone')
		useSessionMethodsForExtensionsForPackageNamed: packageName
]
