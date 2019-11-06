Extension { #name : 'RwProjectDefinitionV2' }

{ #category : '*rowan-gemstone-components-extensions' }
RwProjectDefinitionV2 >> asLoadedSymbolDictProject [

	^ RwGsLoadedSymbolDictComponentProject newForProjectReferenceDefinition: self projectRef.
]

{ #category : '*rowan-gemstone-components-extensions' }
RwProjectDefinitionV2 >> exportTopazFormatTo: filePath [
	| projectSetDefinition projectSetModification visitor fileReference |
	fileReference := filePath asFileReference.
	projectSetDefinition := RwProjectSetDefinition new.
	projectSetDefinition addDefinition: self.
	projectSetModification := projectSetDefinition
		compareAgainstBase: RwProjectSetDefinition new.
	visitor := RwGsModificationTopazWriterVisitor new
		repositoryRootPath: fileReference parent;
		topazFilename: fileReference basename;
		yourself.
	visitor visit: projectSetModification
]

{ #category : '*rowan-gemstone-components-extensions' }
RwProjectDefinitionV2 >> exportTopazFormatTo: filePath usingPackageNamesMap: packageNamesMap [
	| projectSetDefinition projectSetModification visitor fileReference |
	fileReference := filePath asFileReference.
	projectSetDefinition := RwProjectSetDefinition new.
	projectSetDefinition addDefinition: self.
	projectSetModification := projectSetDefinition
		compareAgainstBase: RwProjectSetDefinition new.
	visitor := RwGsModificationTopazWriterVisitor new
		repositoryRootPath: fileReference parent;
		topazFilename: fileReference basename;
		topazFilenamePackageNamesMap: packageNamesMap;
		yourself.
	visitor visit: projectSetModification
]

{ #category : '*rowan-gemstone-components-extensions' }
RwProjectDefinitionV2 >> methodEnvForPackageNamed: packageName [

	^self projectRef methodEnvForPackageNamed: packageName
]

{ #category : '*rowan-gemstone-components-extensions' }
RwProjectDefinitionV2 >> setDefaultSymbolDictionary: symDictName forUser: userId [

	(self componentNamed: self defaultComponentName)
		conditionalPackageMapSpecsAtGemStoneUserId: userId 
			setDefaultSymbolDictNameTo: symDictName
]

{ #category : '*rowan-gemstone-components-extensions' }
RwProjectDefinitionV2 >> setSymbolDictName: symbolDictName forPackageNamed: packageName [

	^self projectRef setSymbolDictName: symbolDictName forPackageNamed: packageName
]

{ #category : '*rowan-gemstone-components-extensions' }
RwProjectDefinitionV2 >> setUseSessionMethodsForExtensions: aBool forPackageNamed: packageName [

	self projectRef
		setUseSessionMethodsForExtensions: aBool
		forPackageNamed: packageName
]

{ #category : '*rowan-gemstone-components-extensions' }
RwProjectDefinitionV2 >> symbolDictNameForPackageNamed: packageName [

	^self projectRef symbolDictNameForPackageNamed: packageName
]

{ #category : '*rowan-gemstone-components-extensions' }
RwProjectDefinitionV2 >> updateGsPlatformLoadedComponentInfoFor: aLoadedProject from: projectInfo [

	|  thePackageMapSpecs |
	thePackageMapSpecs := projectInfo at:  'packageMapSpecs' .
	(thePackageMapSpecs at: #defaultSymbolDictName otherwise: nil) 
		ifNotNil: [:name | aLoadedProject defaultSymbolDictName: name ].
	(thePackageMapSpecs at: #defaultUseSessionMethodsForExtensions otherwise: nil) 
		ifNotNil: [:boolean | 
			aLoadedProject defaultUseSessionMethodsForExtensions: boolean  ].
	(thePackageMapSpecs at: #packageNameToPlatformPropertiesMap otherwise: nil) 
		ifNotNil: [:map | aLoadedProject packageNameToPlatformPropertiesMap: map]
]

{ #category : '*rowan-gemstone-components-extensions' }
RwProjectDefinitionV2 >> updateGsPlatformSpecLoadedProjectInfo: projectInfo [

	| thePackageMapSpecs |
	thePackageMapSpecs := projectInfo at:  'packageMapSpecs' .
	(thePackageMapSpecs at: #defaultSymbolDictName otherwise: nil) 
		ifNotNil: [:name | self defaultSymbolDictName: name ].
	(thePackageMapSpecs at: #defaultUseSessionMethodsForExtensions otherwise: nil) 
		ifNotNil: [:boolean | 
			self defaultUseSessionMethodsForExtensions: boolean  ].
	(thePackageMapSpecs at: #packageNameToPlatformPropertiesMap otherwise: nil) 
		ifNotNil: [:map | self packageNameToPlatformPropertiesMap: map]
]

{ #category : '*rowan-gemstone-components-extensions' }
RwProjectDefinitionV2 >> useSessionMethodsForExtensionsForPackageNamed: packageName [

	^ self projectRef useSessionMethodsForExtensionsForPackageNamed: packageName
]
