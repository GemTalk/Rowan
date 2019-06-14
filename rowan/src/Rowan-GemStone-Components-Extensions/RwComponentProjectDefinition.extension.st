Extension { #name : 'RwComponentProjectDefinition' }

{ #category : '*rowan-gemstone-components-extensions' }
RwComponentProjectDefinition >> asLoadedSymbolDictProject [

	^ RwGsLoadedSymbolDictComponentProject newForProjectReferenceDefinition: self projectRef.
]

{ #category : '*rowan-gemstone-components-extensions' }
RwComponentProjectDefinition >> defaultSymbolDictName [

	^ self projectRef defaultSymbolDictName
]

{ #category : '*rowan-gemstone-components-extensions' }
RwComponentProjectDefinition >> defaultSymbolDictName: symDictName [

	self projectDefinitionSourceProperty: nil.	"when project definition is loaded,each package needs to update it's target symbol dictionary"
	self projectRef defaultSymbolDictName: symDictName
]

{ #category : '*rowan-gemstone-components-extensions' }
RwComponentProjectDefinition >> defaultUseSessionMethodsForExtensions [

	^ self projectRef defaultUseSessionMethodsForExtensions
]

{ #category : '*rowan-gemstone-components-extensions' }
RwComponentProjectDefinition >> defaultUseSessionMethodsForExtensions: aBool [

	self projectRef defaultUseSessionMethodsForExtensions: aBool
]

{ #category : '*rowan-gemstone-components-extensions' }
RwComponentProjectDefinition >> methodEnvForPackageNamed: packageName [

	^self projectRef methodEnvForPackageNamed: packageName
]

{ #category : '*rowan-gemstone-components-extensions' }
RwComponentProjectDefinition >> packageNameToPlatformPropertiesMap [

	^self projectRef packageNameToPlatformPropertiesMap
]

{ #category : '*rowan-gemstone-components-extensions' }
RwComponentProjectDefinition >> packageNameToPlatformPropertiesMap: aDictionary [

	^self projectRef packageNameToPlatformPropertiesMap: aDictionary
]

{ #category : '*rowan-gemstone-components-extensions' }
RwComponentProjectDefinition >> register [

	Rowan image 
		loadedProjectNamed: self name
		ifAbsent: [
			| newLoadedProject |
			newLoadedProject := RwGsLoadedSymbolDictComponentProject newForProjectReferenceDefinition: self projectRef.
			Rowan image addLoadedProject: newLoadedProject ]
]

{ #category : '*rowan-gemstone-components-extensions' }
RwComponentProjectDefinition >> setSymbolDictName: symbolDictName forPackageNamed: packageName [

	self projectDefinitionSourceProperty: nil.	"when project definition is loaded,each package needs to update it's target symbol dictionary"
	^self projectRef setSymbolDictName: symbolDictName forPackageNamed: packageName
]

{ #category : '*rowan-gemstone-components-extensions' }
RwComponentProjectDefinition >> symbolDictNameForPackageNamed: packageName [

	^self projectRef symbolDictNameForPackageNamed: packageName
]

{ #category : '*rowan-gemstone-components-extensions' }
RwComponentProjectDefinition >> updateGsPlatformSpecLoadedProjectInfo: projectInfo [

	| thePackageMapSpecs |
	self projectDefinitionSourceProperty: nil.	"when project definition is loaded,each package needs to update it's target symbol dictionary"
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
RwComponentProjectDefinition >> useSessionMethodsForExtensionsForPackageNamed: packageName [

	^ self projectRef useSessionMethodsForExtensionsForPackageNamed: packageName
]
