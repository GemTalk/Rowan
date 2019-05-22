Class {
	#name : 'RwComponentProjectDefinition',
	#superclass : 'RwProjectDefinition',
	#instVars : [
		'components'
	],
	#category : 'Rowan-Components'
}

{ #category : 'instance creation' }
RwComponentProjectDefinition class >> newForDiskBasedProjectNamed: projectName [

	^ self newForProjectReference: (RwProjectReferenceDefinition
		projectName: projectName 
			useGit: false )
]

{ #category : 'instance creation' }
RwComponentProjectDefinition class >> newForGitBasedProjectNamed: projectName [

	^ self newForProjectReference: (RwProjectReferenceDefinition
		projectName: projectName 
			useGit: true )
]

{ #category : 'instance creation' }
RwComponentProjectDefinition class >> newForProjectReference: aRwProjectReferenceDefintion [

	^ self
		withProperties:
			(Dictionary new
				add: 'name' -> aRwProjectReferenceDefintion projectAlias;
				add: 'projectRef' -> aRwProjectReferenceDefintion;
				yourself)
		packageDefinitions: Dictionary new
		componentDefinitions: Dictionary new
]

{ #category : 'instance creation' }
RwComponentProjectDefinition class >> newForSpecification: aRwSpecification [

	^ RwComponentProjectDefinition newForProjectReference: (RwProjectReferenceDefinition newForSpecification: aRwSpecification)
]

{ #category : 'instance creation' }
RwComponentProjectDefinition class >> newForUrl: specUrl [

	^ self newForSpecification: specUrl asRwUrl asSpecification
]

{ #category : 'instance creation' }
RwComponentProjectDefinition class >> projectName: projectName componentNamesToLoad: componentNamesToLoad groupNamesToLoad: groupNamesToLoad defaultComponentName: defaultComponentName defaultGroupName: defaultGroupName packageFormat: packageFormat projectHome: projectHomeFileReferenceOrString specsPath: specsPath componentsPath: componentsPath packagesPath: packagesPath projectsPath: projectsPath projectUrl: projectUrl committish: commitish committishType: committishType comment: comment [

	| projectRef |
	projectRef := RwProjectReferenceDefinition new
		projectName: projectName;
		configurationNames: componentNamesToLoad;
		defaultComponentName: defaultComponentName;
		groupNames: groupNamesToLoad;
		useGit: true;
		comment: comment;
		projectHome: projectHomeFileReferenceOrString;
		configsPath: componentsPath;
		packagesPath: packagesPath;
		projectsPath: projectsPath;
		specsPath: specsPath;
		packageFormat: packageFormat;
		defaultGroupName: defaultGroupName;
		projectUrl: projectUrl;
		committish: commitish
			committishType: committishType;
		yourself.
	^ self newForProjectReference: projectRef
]

{ #category : 'instance creation' }
RwComponentProjectDefinition class >> projectName: projectName componentNamesToLoad: componentNamesToLoad groupNamesToLoad: groupNamesToLoad defaultComponentName: defaultComponentName defaultGroupName: defaultGroupName packageFormat: packageFormat projectHome: projectHomeFileReferenceOrString specsPath: specsPath componentsPath: componentsPath packagesPath: packagesPath projectsPath: projectsPath useGit: useGit comment: comment [

	| projectRef |
	projectRef := RwProjectReferenceDefinition new
		projectName: projectName;
		configurationNames: componentNamesToLoad;
		defaultComponentName: defaultComponentName;
		groupNames: groupNamesToLoad;
		useGit: useGit;
		comment: comment;
		projectHome: projectHomeFileReferenceOrString;
		configsPath: componentsPath;
		packagesPath: packagesPath;
		projectsPath: projectsPath;
		specsPath: specsPath;
		packageFormat: packageFormat;
		defaultGroupName: defaultGroupName;
		yourself.
	^ self newForProjectReference: projectRef
]

{ #category : 'deprecated' }
RwComponentProjectDefinition class >> projectName: projectName configurationNames: configurationNames groupNames: groupNames defaultComponentName: defaultComponentName useGit: useGit projectUrl: projectUrl projectHome: projectHomeFileReferenceOrString committish: committish committishType: committishType comment: comment [


	^ self newForProjectReference: (RwProjectReferenceDefinition
		projectName: projectName 
			configurationNames: configurationNames 
			groupNames: groupNames 
			defaultComponentName: defaultComponentName
			useGit: useGit 
			projectUrl: projectUrl 
			projectHome: projectHomeFileReferenceOrString
			committish: committish 
			committishType: committishType 
			comment: comment)
]

{ #category : 'deprecated' }
RwComponentProjectDefinition class >> projectName: projectName configurationNames: configurationNames groupNames: groupNames useGit: useGit projectUrl: projectUrl comment: comment [


	^ self newForProjectReference: (RwProjectReferenceDefinition
		projectName: projectName 
			configurationNames: configurationNames 
			groupNames: groupNames 
			useGit: useGit 
			projectUrl: projectUrl 
			comment: comment)
]

{ #category : 'deprecated' }
RwComponentProjectDefinition class >> projectName: projectName configurationNames: configurationNames groupNames: groupNames useGit: useGit projectUrl: projectUrl committish: committish committishType: committishType comment: comment [

	^ self newForProjectReference: (RwProjectReferenceDefinition
		projectName: projectName 
			configurationNames: configurationNames 
			groupNames: groupNames 
			useGit: useGit 
			projectUrl: projectUrl 
			committish: committish 
			committishType: committishType 
			comment: comment)
]

{ #category : 'deprecated' }
RwComponentProjectDefinition class >> projectName: projectName configurationNames: configurationNames groupNames: groupNames useGit: useGit projectUrl: projectUrl projectHome: projectHomeFileReferenceOrString committish: committish committishType: committishType comment: comment [


	^ self newForProjectReference: (RwProjectReferenceDefinition
		projectName: projectName 
			configurationNames: configurationNames 
			groupNames: groupNames 
			useGit: useGit 
			projectUrl: projectUrl 
			projectHome: projectHomeFileReferenceOrString
			committish: committish 
			committishType: committishType 
			comment: comment)
]

{ #category : 'instance creation' }
RwComponentProjectDefinition class >> projectName: projectName projectHome: projectHomeFileReferenceOrString useGit: useGit comment: comment [

	| projectRef |
	projectRef := RwProjectReferenceDefinition new
		projectName: projectName;
		useGit: useGit;
		comment: comment;
		projectHome: projectHomeFileReferenceOrString;
		yourself.
	^ self newForProjectReference: projectRef
]

{ #category : 'instance creation' }
RwComponentProjectDefinition class >> withProperties: properties packageDefinitions: packageDefinitions componentDefinitions: componentDefinitions [

	^ self basicNew
		properties: properties;
		packages: packageDefinitions;
		components: componentDefinitions;
		yourself
]

{ #category : 'visiting' }
RwComponentProjectDefinition >> acceptVisitor: aVisitor [

	^ aVisitor visitComponentProjectDefinition: self
]

{ #category : 'accessing' }
RwComponentProjectDefinition >> addComponentNamed: aComponentName comment: commentString [

	| component |
	component := self 
		components at: aComponentName 
		ifAbsentPut: [ RwComponentLoadConfiguration newNamed: aComponentName for: self name ].
	^ component 
		addDefinedGroupName: self defaultGroupName includeGroups: #();
		comment: commentString;
		yourself
]

{ #category : 'accessing' }
RwComponentProjectDefinition >> addComponentNamed: aComponentName definedGroupNames: groupNameDict gemstoneDefaultSymbolDictionaryForUser: aSymbolDictAssoc comment: commentString [

	| component |
	component := self 
		components at: aComponentName 
		ifAbsentPut: [ RwComponentLoadConfiguration newNamed: aComponentName for: self name ].
	groupNameDict keysAndValuesDo: [:groupName :includeGroups |
		component addDefinedGroupName: groupName includeGroups: includeGroups ].
	component
		conditionalPackageMapSpecsAtGemStoneUserId: aSymbolDictAssoc key 
			setDefaultSymbolDictNameTo: aSymbolDictAssoc value;
		comment: commentString.
	^ component
]

{ #category : 'accessing' }
RwComponentProjectDefinition >> addPackageNamed: packageName [

	^ self
		addPackageNamed: packageName 
			toComponentNamed: self defaultComponentName 
			withConditions: #( 'common' ) 
			andGroupName: self defaultGroupName
]

{ #category : 'accessing' }
RwComponentProjectDefinition >> addPackageNamed: packageName toComponentNamed: componentName withConditions: conditionArray andGroupName: groupName [

	| package component |
	package := super addPackageNamed: packageName.
	component := self 
		components at: componentName 
		ifAbsentPut: [ RwComponentLoadConfiguration newNamed: componentName for: self name ].
	component
		conditionalPackagesAtConditions: conditionArray
			andGroup: groupName
			addPackageNames: { packageName }.
	^package
]

{ #category : 'accessing' }
RwComponentProjectDefinition >> addPackageNamed: packageName toComponentNamed: componentName withConditions: conditionArray andGroupName: groupName  gemstoneDefaultSymbolDictionaryForUser: aSymbolDictAssoc [

	| package component |
	package := super addPackageNamed: packageName.
	component := self 
		components at: componentName 
		ifAbsentPut: [ RwComponentLoadConfiguration newNamed: componentName for: self name ].
	component
		conditionalPackageMapSpecsAtGemStoneUserId: aSymbolDictAssoc key
			andPackageName: packageName
			setSymbolDictNameTo: aSymbolDictAssoc value;
		conditionalPackagesAtConditions: conditionArray
			andGroup: groupName
			addPackageNames: { packageName }.
	^package
]

{ #category : 'accessing' }
RwComponentProjectDefinition >> addPackageNamed: packageName toComponentNamed: componentName withConditions: conditionArray gemstoneDefaultSymbolDictionaryForUser: aSymbolDictAssoc [

	| package component |
	package := super addPackageNamed: packageName.
	component := self 
		components at: componentName 
		ifAbsentPut: [ RwComponentLoadConfiguration newNamed: componentName for: self name ].
	component
		conditionalPackageMapSpecsAtGemStoneUserId: aSymbolDictAssoc key
			andPackageName: packageName 
			setSymbolDictNameTo: aSymbolDictAssoc value;
		conditionalPackagesAtConditions: conditionArray
			andGroup: self defaultGroupName
			addPackageNames: { packageName }.
	^package
]

{ #category : 'accessing' }
RwComponentProjectDefinition >> addPackageNamed: packageName withConditions: conditionArray [

	| package component |
	package := self
		addPackageNamed: packageName 
			toComponentNamed: self defaultComponentName 
			withConditions: conditionArray
			andGroupName: self defaultGroupName.
	component := self 
		components at: self defaultComponentName 
		ifAbsentPut: [ RwComponentLoadConfiguration newNamed: self defaultComponentName for: self name ].
	component
		conditionalPackagesAtConditions: conditionArray
			andGroup: self defaultGroupName
			addPackageNames: { packageName }.
	^package
]

{ #category : 'accessing' }
RwComponentProjectDefinition >> addPackageNamed: packageName withConditions: conditionArray andGroupName: groupName [

	| package component |
	package := self
		addPackageNamed: packageName 
			toComponentNamed: self defaultComponentName 
			withConditions: conditionArray
			andGroupName: groupName.
	component := self 
		components at: self defaultComponentName 
		ifAbsentPut: [ RwComponentLoadConfiguration newNamed: self defaultComponentName for: self name ].
	component
		conditionalPackagesAtConditions: conditionArray
			andGroup: groupName
			addPackageNames: { packageName }.
	^package
]

{ #category : 'accessing' }
RwComponentProjectDefinition >> addPackageNamed: packageName withConditions: conditionArray andGroupName: groupName gemstoneDefaultSymbolDictionaryForUser: aSymbolDictAssoc [

	| package component |
	package := self
		addPackageNamed: packageName 
			toComponentNamed: self defaultComponentName 
			withConditions: conditionArray
			andGroupName: groupName.
	component := self 
		components at: self defaultComponentName 
		ifAbsentPut: [ RwComponentLoadConfiguration newNamed: self defaultComponentName for: self name ].
	component
		conditionalPackageMapSpecsAtGemStoneUserId: aSymbolDictAssoc key
			andPackageName: packageName
			setSymbolDictNameTo: aSymbolDictAssoc value;
		conditionalPackagesAtConditions: conditionArray
			andGroup: groupName
			addPackageNames: { packageName }.
	^package
]

{ #category : 'accessing' }
RwComponentProjectDefinition >> addPackageNamed: packageName withConditions: conditionArray gemstoneDefaultSymbolDictionaryForUser: aSymbolDictAssoc [

	| package component |
	package := self
		addPackageNamed: packageName 
			toComponentNamed: self defaultComponentName 
			withConditions: conditionArray
			andGroupName: self defaultGroupName.
	component := self 
		components at: self defaultComponentName 
		ifAbsentPut: [ RwComponentLoadConfiguration newNamed: self defaultComponentName for: self name ].
	component
		conditionalPackageMapSpecsAtGemStoneUserId: aSymbolDictAssoc key 
			andPackageName: packageName
			setSymbolDictNameTo: aSymbolDictAssoc value.
	^package
]

{ #category : 'accessing' }
RwComponentProjectDefinition >> addPackagesNamed: packageNames [

	^ packageNames collect: [:packageName | 
		self addPackageNamed: packageName ]
]

{ #category : 'accessing' }
RwComponentProjectDefinition >> addPackagesNamed: packageNames toComponentNamed: componentName withConditions: conditionArray andGroupName: groupName [


	^ packageNames collect: [:packageName | 
		self addPackageNamed: packageName toComponentNamed: componentName withConditions: conditionArray andGroupName: groupName ]
]

{ #category : 'accessing' }
RwComponentProjectDefinition >> addPackagesNamed: packageNames toComponentNamed: componentName withConditions: conditionArray andGroupName: groupName  gemstoneDefaultSymbolDictionaryForUser: aSymbolDictAssoc [

	^ packageNames collect: [:packageName | 
		self addPackageNamed: packageName toComponentNamed: componentName withConditions: conditionArray andGroupName: groupName  gemstoneDefaultSymbolDictionaryForUser: aSymbolDictAssoc ]
]

{ #category : 'accessing' }
RwComponentProjectDefinition >> addPackagesNamed: packageNames toComponentNamed: componentName withConditions: conditionArray gemstoneDefaultSymbolDictionaryForUser: aSymbolDictAssoc [

	^ packageNames collect: [:packageName | 
		self addPackageNamed: packageName toComponentNamed: componentName withConditions: conditionArray gemstoneDefaultSymbolDictionaryForUser: aSymbolDictAssoc ]
]

{ #category : 'accessing' }
RwComponentProjectDefinition >> addPackagesNamed: packageNames withConditions: conditionArray [

	^ packageNames collect: [:packageName |
			self addPackageNamed: packageName withConditions: conditionArray ]
]

{ #category : 'accessing' }
RwComponentProjectDefinition >> addPackagesNamed: packageNames withConditions: conditionArray andGroupName: groupName [

	^ packageNames collect: [:packageName | 
		self addPackageNamed: packageName withConditions: conditionArray andGroupName: groupName ]
]

{ #category : 'accessing' }
RwComponentProjectDefinition >> addPackagesNamed: packageNames withConditions: conditionArray andGroupName: groupName gemstoneDefaultSymbolDictionaryForUser: aSymbolDictAssoc [

	^ packageNames collect: [:packageName | 
		self addPackageNamed: packageName withConditions: conditionArray andGroupName: groupName gemstoneDefaultSymbolDictionaryForUser: aSymbolDictAssoc ]
]

{ #category : 'accessing' }
RwComponentProjectDefinition >> addPackagesNamed: packageNames withConditions: conditionArray gemstoneDefaultSymbolDictionaryForUser: aSymbolDictAssoc [ 

	^ packageNames collect: [:packageName | 
		self addPackageNamed: packageName withConditions: conditionArray gemstoneDefaultSymbolDictionaryForUser: aSymbolDictAssoc ]
]

{ #category : 'actions' }
RwComponentProjectDefinition >> clone [

	self projectRef clone
]

{ #category : 'accessing' }
RwComponentProjectDefinition >> componentNamed: aComponentName [

	^ self componentNamed: aComponentName ifAbsent: [ self error: 'The component named ', aComponentName printString, ' was not found' ]
]

{ #category : 'accessing' }
RwComponentProjectDefinition >> componentNamed: aComponentName ifAbsent: absentBlock [

	^ self components 
		at: aComponentName 
		ifAbsent: absentBlock
]

{ #category : 'accessing' }
RwComponentProjectDefinition >> components [

	^ components
]

{ #category : 'accessing' }
RwComponentProjectDefinition >> components: aComponentDefinitionDictionary [

	components := aComponentDefinitionDictionary
]

{ #category : 'properties' }
RwComponentProjectDefinition >> configsPath [

	^ self projectRef configsPath
]

{ #category : 'properties' }
RwComponentProjectDefinition >> configsPath: aDirectoryPathString [

	self projectRef configsPath: aDirectoryPathString
]

{ #category : 'properties' }
RwComponentProjectDefinition >> configsRoot [
	^ self projectRef configsRoot
]

{ #category : 'actions' }
RwComponentProjectDefinition >> create [
	"write the project structure to disk, if it doesn't already exist"

	Rowan projectTools create
		createComponentProject: self.
]

{ #category : 'accessing' }
RwComponentProjectDefinition >> defaultComponentName [

	^ self projectRef defaultComponentName
]

{ #category : 'accessing' }
RwComponentProjectDefinition >> defaultComponentName: aString [

	^ self projectRef defaultComponentName: aString
]

{ #category : 'properties' }
RwComponentProjectDefinition >> defaultConfigurationNames [

	^ self propertyAt: 'defaultConfigurationNames' ifAbsent: [ self projectRef configurationNames ]
]

{ #category : 'accessing' }
RwComponentProjectDefinition >> defaultGroupName [

	^ self projectRef defaultGroupName
]

{ #category : 'accessing' }
RwComponentProjectDefinition >> defaultGroupName: aString [

	^ self projectRef defaultGroupName: aString
]

{ #category : 'properties' }
RwComponentProjectDefinition >> defaultGroupNames [

	^ self projectRef groupNames
]

{ #category : 'accessing' }
RwComponentProjectDefinition >> defaultSymbolDictionary: symDictName forUser: userId [

	(self componentNamed: self defaultComponentName)
		conditionalPackageMapSpecsAtGemStoneUserId: userId 
			setDefaultSymbolDictNameTo: symDictName
]

{ #category : 'actions' }
RwComponentProjectDefinition >> export [

	Rowan projectTools write writeComponentProjectDefinition: self
]

{ #category : 'exporting' }
RwComponentProjectDefinition >> exportComponents [

	self components values do: [:component|
		component exportToUrl: 'file:',  self configsRoot pathString, '/' ].
	self components isEmpty
		ifTrue: [
			"add README.md as placeholder to ensure that the directory is preserved by git"
			(self configsRoot /  'README', 'md') writeStreamDo: [ :fileStream | ] ]
]

{ #category : 'exporting' }
RwComponentProjectDefinition >> exportPackages [
	"attempt to do incremental write to disk, however, if disk cannot be read, write all packages to disk"
	| projectSetDefinition |
	projectSetDefinition := [ (self class newForProjectReference: self projectRef) readProjectSet ]
		on: Error
		do: [:ignored | RwProjectSetDefinition new ].
	self exportPackages: projectSetDefinition.
]

{ #category : 'exporting' }
RwComponentProjectDefinition >> exportPackages: diskProjectSetDefinition [

	| projectSetDefinition visitor projectSetModification writerVisitorClass |
	(self packagesRoot /  'properties', 'st') 
		writeStreamDo: [ :fileStream | 
			fileStream nextPutAll: '{ '; 
				lf;
				tab;
				nextPutAll: '#format : ' , self packageFormat printString, ',' ;
				lf;
				tab;
				nextPutAll: '#convention : ', self packageConvention printString;
				lf;
				nextPutAll: '}';
				lf ].

"write out packages"
	writerVisitorClass := self packageFormat = 'tonel'
		ifTrue: [ RwModificationTonelWriterVisitor ]
		ifFalse: [ RwModificationFiletreeWriterVisitor ].
	projectSetDefinition:= RwProjectSetDefinition new.
	projectSetDefinition addDefinition: self.
	projectSetModification := projectSetDefinition compareAgainstBase: diskProjectSetDefinition.
	visitor := writerVisitorClass new.

	visitor visit: projectSetModification.
]

{ #category : 'exporting' }
RwComponentProjectDefinition >> exportProjects [

	self projectNames do: [:projectName |
		self error: 'not yet implemented' ].
	self projectNames isEmpty
		ifTrue: [
			"add README.md as placeholder to ensure that the directory is preserved by git"
			(self projectsRoot /  'README', 'md') writeStreamDo: [ :fileStream | ] ]
]

{ #category : 'accessing' }
RwComponentProjectDefinition >> gitRoot: aGitRootReferenceOrString [ 

	^ self projectRef gitRoot: aGitRootReferenceOrString
]

{ #category : 'accessing' }
RwComponentProjectDefinition >> loadedCommitId [

	^ self projectRef loadedCommitId
]

{ #category : 'properties' }
RwComponentProjectDefinition >> loadedConfigurationNames [

	^ self projectRef loadedConfigurationNames
]

{ #category : 'properties' }
RwComponentProjectDefinition >> loadedConfigurationNames: anArray [

	^ self projectRef loadedConfigurationNames: anArray
]

{ #category : 'properties' }
RwComponentProjectDefinition >> loadedGroupNames [

	^ self projectRef loadedGroupNames
]

{ #category : 'properties' }
RwComponentProjectDefinition >> loadedGroupNames: anArray [

	^ self projectRef loadedGroupNames: anArray
]

{ #category : 'accessing' }
RwComponentProjectDefinition >> packageConvention [

	^ self projectRef packageConvention
]

{ #category : 'accessing' }
RwComponentProjectDefinition >> packageConvention: aString [

	self projectRef packageConvention: aString
]

{ #category : 'accessing' }
RwComponentProjectDefinition >> packageFormat [

	^ self projectRef packageFormat
]

{ #category : 'accessing' }
RwComponentProjectDefinition >> packageFormat: aString [

	self projectRef packageFormat: aString
]

{ #category : 'accessing' }
RwComponentProjectDefinition >> packageFormatIfAbsent: aBlock [

	^ self projectRef packageFormatIfAbsent: aBlock
]

{ #category : 'properties' }
RwComponentProjectDefinition >> packagesPath [

	^ self projectRef packagesPath
]

{ #category : 'properties' }
RwComponentProjectDefinition >> packagesPath: aString [

	^ self projectRef packagesPath: aString
]

{ #category : 'properties' }
RwComponentProjectDefinition >> packagesRoot [

	^ self projectRef packagesRoot
]

{ #category : 'copying' }
RwComponentProjectDefinition >> postCopy [

	super postCopy.
	components := components copy.
	components keysAndValuesDo: [:key : value | components at: key put: value copy ] .
]

{ #category : 'accessing' }
RwComponentProjectDefinition >> projectHome [

	^ self projectRef projectHome
]

{ #category : 'accessing' }
RwComponentProjectDefinition >> projectHome: aProjectHomeReferenceOrString [

	^ self projectRef projectHome: aProjectHomeReferenceOrString
]

{ #category : 'accessing' }
RwComponentProjectDefinition >> projectNames [

	^ self projectRef projectNames
]

{ #category : 'accessing' }
RwComponentProjectDefinition >> projectNames: anArray [

	self projectRef projectNames: anArray
]

{ #category : 'accessing' }
RwComponentProjectDefinition >> projectRef [

	^ self propertyAt: 'projectRef' ifAbsent: [ ]
]

{ #category : 'accessing' }
RwComponentProjectDefinition >> projectRef: aRwProjectReferenceDefinition [

	^ self propertyAt: 'projectRef' put: aRwProjectReferenceDefinition
]

{ #category : 'properties' }
RwComponentProjectDefinition >> projectsPath [

	^ self projectRef projectsPath
]

{ #category : 'properties' }
RwComponentProjectDefinition >> projectsPath: aString [

	^ self projectRef projectsPath: aString
]

{ #category : 'properties' }
RwComponentProjectDefinition >> projectsRoot [

	^ self projectRef projectsRoot
]

{ #category : 'tool api' }
RwComponentProjectDefinition >> readProjectSet [

	^ Rowan projectTools read  readProjectSetForComponentProjectDefinition: self
]

{ #category : 'actions' }
RwComponentProjectDefinition >> readProjectSetForPackageNames: packageNames [

	| format visitorClass |
	format := self 
		packageFormatIfAbsent: [  
			| formatFromDisk |
			formatFromDisk := (RwAbstractReaderWriterVisitor _repositoryPropertyDictFor: self packagesRoot)
				at: #format ifAbsent: [ 'tonel' ].
			self packageFormat: formatFromDisk.
			formatFromDisk ].
	visitorClass := format = 'tonel'
		ifTrue: [ RwRepositoryComponentProjectTonelReaderVisitor ]
		ifFalse: [ RwRepositoryComponentProjectFiletreeReaderVisitor ].
	^ visitorClass new
		packageNames: packageNames;
		visit: self.
]

{ #category : 'reading' }
RwComponentProjectDefinition >> readProjectSetReadTool: readTool withConfigurations: theConfigNames groupNames: theGroupNames [

	^ readTool
		readProjectSetForComponentProjectDefinition: self
			withConfigurations: theConfigNames 
			groupNames: theGroupNames
]

{ #category : 'temporary compat' }
RwComponentProjectDefinition >> repoPath [

	"use packagesPath"
	^ self shouldNotImplement: #repoPath
]

{ #category : 'temporary compat' }
RwComponentProjectDefinition >> repoPath: astring [

	"use packagesPath:"
	^ self shouldNotImplement: #repoPath:
]

{ #category : 'properties' }
RwComponentProjectDefinition >> repositoryRoot [
	"Root directory of the project. The configsPath, repoPath, specsPath, and projectsPath are specified relative to the repository root."

	^ self projectRef repositoryRoot
]

{ #category : 'properties' }
RwComponentProjectDefinition >> repositoryRoot: aFileReferenceOrPath [
	"Root directory of the project. The configsPath, repoPath, specsPath, and projectsPath are specified relative to the repository root."

	^ self projectRef repositoryRoot: aFileReferenceOrPath
]

{ #category : 'temporary compat' }
RwComponentProjectDefinition >> repositoryRootPath [

	^ self repositoryRoot fullName
]

{ #category : 'temporary compat' }
RwComponentProjectDefinition >> specification [

self deprecated: 'temporary patch .. sender should be replaced with projectRef'.
	^ self projectRef asSpecification
]

{ #category : 'properties' }
RwComponentProjectDefinition >> specsPath [

	^ self projectRef specsPath
]

{ #category : 'properties' }
RwComponentProjectDefinition >> specsPath: aDirectoryPathString [

	self projectRef specsPath: aDirectoryPathString
]

{ #category : 'properties' }
RwComponentProjectDefinition >> specsRoot [

	^ self projectRef specsRoot
]

{ #category : 'loading' }
RwComponentProjectDefinition >> updateLoadedCommitId [

	self projectRef updateLoadedCommitId
]

{ #category : 'properties' }
RwComponentProjectDefinition >> useGit [

	^ self projectRef useGit
]
