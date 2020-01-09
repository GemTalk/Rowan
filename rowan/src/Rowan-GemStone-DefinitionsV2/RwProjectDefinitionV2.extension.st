Extension { #name : 'RwProjectDefinitionV2' }

{ #category : '*rowan-gemstone-definitionsv2' }
RwProjectDefinitionV2 >> _gemstoneAllUsersName [
	^ RwLoadSpecificationV2 _gemstoneAllUsersName
]

{ #category : '*rowan-gemstone-definitionsv2' }
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

{ #category : '*rowan-gemstone-definitionsv2' }
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

{ #category : '*rowan-gemstone-definitionsv2' }
RwProjectDefinitionV2 >> gemstoneDefaultSymbolDictNameForUser: userId [
	^ self projectSpecification gemstoneDefaultSymbolDictNameForUser: userId
]

{ #category : '*rowan-gemstone-definitionsv2' }
RwProjectDefinitionV2 >> gemstoneSetSymbolDictName: symbolDictName forPackageNamed: packageName [
	"answer true if the package was found in one of the receivers components"

	| foundOne |
	foundOne := false.
	self components
		keysAndValuesDo: [ :componentName :component | 
			(component packageNames includes: packageName)
				ifTrue: [ 
					foundOne := true.
					component
						conditionalPackageMapSpecsAtGemStoneUserId: self _gemstoneAllUsersName
						andPackageName: packageName
						setSymbolDictNameTo: symbolDictName ] ].
	foundOne ifFalse: [ self error: 'No package named ' , packageName printString , ' found.']
]

{ #category : '*rowan-gemstone-definitionsv2' }
RwProjectDefinitionV2 >> gemstoneSymbolDictNameForPackageNamed: packageName forUser: userId ifAbsent: absentBlock [
	self components
		keysAndValuesDo: [ :componentName :component | 
			(component packageNames includes: packageName)
				ifTrue: [ 
					| userIdPropertiesMap packagePropertiesMap |
					userIdPropertiesMap := (component
						conditionalPackageMapSpecsAtGemStoneUserId: userId)
						at: #'packageNameToPlatformPropertiesMap'
						ifAbsent: [ 
							"no entries for the specific userId, check if there's an entry for allusers"
							(component
								conditionalPackageMapSpecsAtGemStoneUserId: self _gemstoneAllUsersName)
								at: #'packageNameToPlatformPropertiesMap'
								ifAbsent: absentBlock ].
					packagePropertiesMap := userIdPropertiesMap
						at: packageName
						ifAbsent: absentBlock.
					^ packagePropertiesMap at: 'symbolDictName' ifAbsent: absentBlock ] ].
	^ absentBlock value
]

{ #category : '*rowan-gemstone-definitionsv2' }
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
