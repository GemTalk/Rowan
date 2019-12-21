Class {
	#name : 'RwResolvedProjectV2',
	#superclass : 'RwAbstractResolvedObjectV2',
	#category : 'Rowan-DefinitionsV2'
}

{ #category : 'instance creation' }
RwResolvedProjectV2 class >> basicLoadSpecification: anRwLoadSpecificationV2 [
	"resolve ensures that the project directory already exists on disk (cloned for git projects) or created on disk for new projects
		answer  the project definition specified by the receiver and any dependent projects"

	"if the project directory already exists on disk, then read the project definition(s) from disk"

	| loadSpecification projectDefinition |
	loadSpecification := anRwLoadSpecificationV2 copy.
	projectDefinition := RwProjectDefinitionV2 basicNew
		properties:
				(Dictionary new
						add: 'name' -> loadSpecification projectAlias;
						yourself);
		packages: Dictionary new;
		components: Dictionary new;
		projectDefinitionSourceProperty: RwLoadedProject _projectSourceValueNewProject;
		yourself.
	^ self basicNew
		_projectDefinition: projectDefinition;
		_loadSpecification: loadSpecification;
		yourself
]

{ #category : 'instance creation' }
RwResolvedProjectV2 class >> loadSpecification: anRwLoadSpecificationV2 [
	"resolve ensures that the project directory already exists on disk (cloned for git projects) or created on disk for new projects
		answer  the project definition specified by the receiver and any dependent projects"

	"if the project directory already exists on disk, then read the project definition(s) from disk"

	^(self basicLoadSpecification: anRwLoadSpecificationV2)
		resolve
]

{ #category : 'private' }
RwResolvedProjectV2 >> _checkProjectDirectoryStructure [
	"answer true if the basic project directory structure is present"

	self projectRoots
		do: [ :fileRef | 
			fileRef exists
				ifFalse: [ ^ false ] ].
	^ true
]

{ #category : 'private' }
RwResolvedProjectV2 >> _loadTool [

	^ self _projectDefinition _loadTool
]

{ #category : 'visiting' }
RwResolvedProjectV2 >> acceptVisitor: aVisitor [
	^ aVisitor visitResolvedProjectV2: self
]

{ #category : 'project definition' }
RwResolvedProjectV2 >> addComponentNamed: aComponentName definedGroupNames: groupNameDict comment: commentString [
	^ self _projectDefinition
		addComponentNamed: aComponentName
		definedGroupNames: groupNameDict
		comment: commentString
]

{ #category : 'project definition' }
RwResolvedProjectV2 >> addPackageNamed: packageName toComponentNamed: componentName withConditions: conditionArray andGroupName: groupName [
	^ self _projectDefinition
		addPackageNamed: packageName
		toComponentNamed: componentName
		withConditions: conditionArray
		andGroupName: groupName
]

{ #category : 'project definition' }
RwResolvedProjectV2 >> addPackageNamed: packageName toComponentNamed: componentName withConditions: conditionArray andGroupName: groupName gemstoneDefaultSymbolDictionaryForUser: aSymbolDictAssoc [
	(self groupNames includes: groupName)
		ifFalse: [ self error: 'The group ' , groupName printString , ' is undefined' ].
	^ self _projectDefinition
		addPackageNamed: packageName
		toComponentNamed: componentName
		withConditions: conditionArray
		andGroupName: groupName
		gemstoneDefaultSymbolDictionaryForUser: aSymbolDictAssoc
]

{ #category : 'project definition' }
RwResolvedProjectV2 >> addPackagesNamed: packageNames toComponentNamed: componentName withConditions: conditionArray andGroupName: groupName [
	(self groupNames includes: groupName)
		ifFalse: [ self error: 'The group ' , groupName printString , ' is undefined' ].
	^ self _projectDefinition
		addPackagesNamed: packageNames
		toComponentNamed: componentName
		withConditions: conditionArray
		andGroupName: groupName
]

{ #category : 'project definition' }
RwResolvedProjectV2 >> comment [
	^ self _projectDefinition comment
]

{ #category : 'project definition' }
RwResolvedProjectV2 >> comment: aString [
	self _projectDefinition comment: aString
]

{ #category : 'project specification' }
RwResolvedProjectV2 >> componentNames [
	^ self _loadSpecification componentNames
]

{ #category : 'project specification' }
RwResolvedProjectV2 >> componentNames: anArray [
	self _loadSpecification componentNames: anArray
]

{ #category : 'project definition' }
RwResolvedProjectV2 >> components [
	^ self _projectDefinition components
]

{ #category : 'copying' }
RwResolvedProjectV2 >> copyForLoadedProject [
	"project definition is not part of the copy for loaded projects"

	^ RwResolvedProjectV2 new
		_projectRepository: projectRepository copy;
		_loadSpecification: loadSpecification copy;
		_projectSpecification: projectSpecification copy;
		_projectStructure: projectDefinition components copy;
		yourself
]

{ #category : 'load specification' }
RwResolvedProjectV2 >> diskUrl [
	^ self _loadSpecification diskUrl
]

{ #category : 'load specification' }
RwResolvedProjectV2 >> diskUrl: anUrlString [
	self _loadSpecification diskUrl: anUrlString
]

{ #category : 'actions' }
RwResolvedProjectV2 >> export [
	Rowan projectTools writeV2 writeResolvedProject: self
]

{ #category : 'exporting' }
RwResolvedProjectV2 >> exportComponents [
	self components isEmpty
		ifTrue: [ self addComponentNamed: self defaultComponentName comment: '' ].
	self components values
		do: [ :component | component exportToUrl: 'file:' , self componentsRoot pathString , '/' ].
	self components isEmpty
		ifTrue: [ 
			"add README.md as placeholder to ensure that the directory is preserved by git"
			self componentsRoot / 'README' , 'md' writeStreamDo: [ :fileStream |  ] ]
]

{ #category : 'exporting' }
RwResolvedProjectV2 >> exportLoadSpecification [
	self _loadSpecification exportTo: self specsRoot
]

{ #category : 'exporting' }
RwResolvedProjectV2 >> exportPackages [
	self _projectDefinition
		exportPackagesTo: self packagesRoot
		packageFormat: self packageFormat
		packageConvention: self packageConvention
]

{ #category : 'exporting' }
RwResolvedProjectV2 >> exportProjects [
	self projectNames do: [ :projectName | self error: 'not yet implemented' ].
	self projectNames isEmpty
		ifTrue: [ 
			"add README.md as placeholder to ensure that the directory is preserved by git"
			self projectsRoot / 'README' , 'md' writeStreamDo: [ :fileStream |  ] ]
]

{ #category : 'exporting' }
RwResolvedProjectV2 >> exportProjectSpecification [
	self _projectSpecification exportTo: self repositoryRoot
]

{ #category : 'load specification' }
RwResolvedProjectV2 >> gitUrl [
	^ self _loadSpecification gitUrl
]

{ #category : 'load specification' }
RwResolvedProjectV2 >> gitUrl: anUrlString [
	self _loadSpecification gitUrl: anUrlString
]

{ #category : 'project specification' }
RwResolvedProjectV2 >> groupNames [
	^ self _loadSpecification groupNames
]

{ #category : 'project specification' }
RwResolvedProjectV2 >> groupNames: anArray [
	self _loadSpecification groupNames: anArray
]

{ #category : 'initialization' }
RwResolvedProjectV2 >> initialize [
	"repository must be explicitly created"

	super initialize.
	projectSpecification := RwProjectSpecificationV2 new.
	loadSpecification := RwLoadSpecificationV2 new.
	projectDefinition := RwProjectDefinitionV2 new
]

{ #category : 'project definition' }
RwResolvedProjectV2 >> isEmpty [
	^ self _projectDefinition isEmpty
]

{ #category : 'project definition' }
RwResolvedProjectV2 >> key [
	^ self projectAlias
]

{ #category : 'actions' }
RwResolvedProjectV2 >> load [
	"load the receiver into the image"

	self _validate: Rowan platformConfigurationAttributes.
	^ Rowan projectTools loadV2 loadProjectDefinition: self projectDefinition
]

{ #category : 'project definition' }
RwResolvedProjectV2 >> loadedCommitId [
	^ self _projectSpecification loadedCommitId
]

{ #category : '-- loader compat --' }
RwResolvedProjectV2 >> name [
	"sender in loader code that's shared between RwComponentProjectDefinition and RwResolvedProjectV2, 
		should use projectAlias, but need to wait until we're no longer using RwComponentProjectDefinition"

	^ self projectAlias
]

{ #category : 'project definition' }
RwResolvedProjectV2 >> packageConvention [
	^ self _projectSpecification packageConvention
]

{ #category : 'project definition' }
RwResolvedProjectV2 >> packageFormat [
	^ self _projectSpecification packageFormat
]

{ #category : 'project definition' }
RwResolvedProjectV2 >> packageFormat: aString [
	^ self _projectSpecification packageFormat: aString
]

{ #category : 'project definition' }
RwResolvedProjectV2 >> packageFormatIfAbsent: absentBlock [
	^ self _projectSpecification packageFormatIfAbsent: absentBlock
]

{ #category : 'project definition' }
RwResolvedProjectV2 >> packageNamed: aString [
	^ self _projectDefinition packageNamed: aString
]

{ #category : 'project definition' }
RwResolvedProjectV2 >> packageNames [
	^ self _projectDefinition packageNames
]

{ #category : 'project definition' }
RwResolvedProjectV2 >> packages [
	^ self _projectDefinition packages
]

{ #category : 'project definition' }
RwResolvedProjectV2 >> packages: aPackageDictionary [
	^ self _projectDefinition packages: aPackageDictionary
]

{ #category : 'accessing' }
RwResolvedProjectV2 >> projectAlias: aString [
	self _loadSpecification projectAlias: aString
]

{ #category : 'project definition' }
RwResolvedProjectV2 >> projectDefinitionSourceProperty [
	^ self _projectDefinition projectDefinitionSourceProperty
]

{ #category : 'project definition' }
RwResolvedProjectV2 >> projectDefinitionSourceProperty: sourceKeyOrNil [
	self _projectDefinition projectDefinitionSourceProperty: sourceKeyOrNil
]

{ #category : 'accessing' }
RwResolvedProjectV2 >> projectName: aString [
	self _loadSpecification projectName: aString.
	self _projectSpecification projectName: aString.
	self _projectDefinition projectName: aString
]

{ #category : 'accessing' }
RwResolvedProjectV2 >> projectNames [
	"not yet implemented"

	^ #()
]

{ #category : 'accessing' }
RwResolvedProjectV2 >> projectsHome: aProjectHomeReferenceOrString [
	self _loadSpecification projectsHome: aProjectHomeReferenceOrString
]

{ #category : 'project definition' }
RwResolvedProjectV2 >> projectUrl [
	^ self _loadSpecification projectUrl
]

{ #category : 'actions' }
RwResolvedProjectV2 >> read [
	"refresh the contents of the receiver ... the reciever will match the definitions on disk based on the current load specification"

	"return a project definition set that will contain the project definition along with any dependent project definitions"

	^ self readComponentNames: self componentNames groupNames: self groupNames
]

{ #category : 'actions' }
RwResolvedProjectV2 >> read: platformConfigurationAttributes [
	"refresh the contents of the receiver ... the reciever will match the definitions on disk based on the current load specification"

	"return a project definition set that will contain the project definition along with any dependent project definitions"

	^ self
		readComponentNames: self componentNames
		groupNames: self groupNames
		platformConfigurationAttributes: platformConfigurationAttributes
]

{ #category : 'actions' }
RwResolvedProjectV2 >> readComponentNames: componentNames groupNames: groupNames [
	"refresh the contents of the receiver ... the reciever will match the definitions on disk based on the current load specification"

	"return a project definition set that will contain the project definition along with any dependent project definitions"

	^ self
		readComponentNames: componentNames
		groupNames: groupNames
		platformConfigurationAttributes: Rowan platformConfigurationAttributes
]

{ #category : 'actions' }
RwResolvedProjectV2 >> readComponentNames: componentNames groupNames: groupNames platformConfigurationAttributes: platformConfigurationAttributes [
	"refresh the contents of the receiver ... the reciever will match the definitions on disk based on the current load specification"

	"return a project definition set that will contain the project definition along with any dependent project definitions"

	^ Rowan projectTools readV2
		readProjectSetForResolvedProject: self
		withComponentNames: componentNames
		groupNames: groupNames
		platformConfigurationAttributes: platformConfigurationAttributes
]

{ #category : 'project definition' }
RwResolvedProjectV2 >> readPackageNames: packageNames [
	| format visitorClass |
	format := self
		packageFormatIfAbsent: [ 
			| formatFromDisk |
			formatFromDisk := (RwAbstractReaderWriterVisitor
				_repositoryPropertyDictFor: self packagesRoot)
				at: #'format'
				ifAbsent: [ 'tonel' ].
			self packageFormat: formatFromDisk.
			formatFromDisk ].
	self _projectDefinition packages: Dictionary new.
	visitorClass := format = 'tonel'
		ifTrue: [ RwRepositoryResolvedProjectTonelReaderVisitorV2 ]
		ifFalse: [ RwRepositoryResolvedProjectFiletreeReaderVisitorV2 ].
	^ visitorClass new
		packageNames: packageNames;
		visit: self
]

{ #category : 'project definition' }
RwResolvedProjectV2 >> removePackageNamed: packageName [
	^ self _projectDefinition removePackageNamed: packageName
]

{ #category : 'project specification' }
RwResolvedProjectV2 >> repoType: aSymbol [
	self _projectSpecification repoType: aSymbol
]

{ #category : 'actions' }
RwResolvedProjectV2 >> resolve [
	"resolve the projectSpecation (clone remote repo or connect to existing repo on disk) and read 
		project from disk, if project is present on disk"

	self _projectRepository resolve
		ifTrue: [ 
			self _projectRepository checkAndUpdateRepositoryRevision: self.
			self _checkProjectDirectoryStructure
				ifTrue: [ 
					"update project definition from disk"
					self read ] ]
]

{ #category : 'load specification' }
RwResolvedProjectV2 >> revision [
	^ self _loadSpecification revision
]

{ #category : 'load specification' }
RwResolvedProjectV2 >> revision: aRevisionString [
	^ self _loadSpecification revision: aRevisionString
]

{ #category : '-- loader compat --' }
RwResolvedProjectV2 >> symbolDictNameForPackageNamed: aPackageName [
	"sender in loader code that's shared between RwComponentProjectDefinition and RwResolvedProjectV2, 
		should use gemstoneSymbolDictNameForPackageNamed:, but need to wait until we're no longer 
		using RwComponentProjectDefinition"

	^ self gemstoneSymbolDictNameForPackageNamed: aPackageName
]

{ #category : 'project definition' }
RwResolvedProjectV2 >> updateLoadedCommitId [
	self _projectSpecification loadedCommitId: self _projectRepository commitId
]
