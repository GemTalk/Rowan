Class {
	#name : 'RwResolvedProjectV2',
	#superclass : 'RwAbstractResolvedObjectV2',
	#category : 'Rowan-Components'
}

{ #category : 'instance creation' }
RwResolvedProjectV2 class >> new [

	^ self basicNew initialize
]

{ #category : 'instance creation' }
RwResolvedProjectV2 class >> projectSpecification: anRwProjectSpecificationV2 [
	"resolve ensures that the project directory already exists on disk (cloned for git projects) or created on disk for new projects
		answer  the project definition specified by the receiver and any dependent projects"

	"if the project directory already exists on disk, then read the project definition(s) from disk"

	| projectSpecification loadSpecification projectDefinition |
	projectSpecification := anRwProjectSpecificationV2 copy.
	loadSpecification := RwLoadSpecificationV2 new.
	projectDefinition := RwProjectDefinitionV2 basicNew
		properties:
				(Dictionary new
						add: 'name' -> projectSpecification projectAlias;
						yourself);
		packages: Dictionary new;
		components: Dictionary new;
		projectDefinitionSourceProperty: RwLoadedProject _projectSourceValueNewProject;
		yourself.
	^ self basicNew
		_projectDefinition: projectDefinition;
		_projectSpecification: projectSpecification;
		_loadSpecification: loadSpecification;
		_resolve
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

{ #category : 'project definition' }
RwResolvedProjectV2 >> commit: message [
	"commit the repository associated with receiver ..."

	self _projectRepository canCommit
		ifFalse: [ 
			| msg |
			msg := 'repository for project ' , self projectAlias printString
				, ' does not support commit operations.'.
			self inform: msg.
			^ msg ].
	^ self _projectDefinition commit: message
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

{ #category : 'project specification' }
RwResolvedProjectV2 >> diskUrl [
	^ self _projectSpecification diskUrl
]

{ #category : 'project specification' }
RwResolvedProjectV2 >> diskUrl: anUrlString [
	self _projectSpecification diskUrl: anUrlString
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

{ #category : 'project specification' }
RwResolvedProjectV2 >> gitUrl [
	^ self _projectSpecification gitUrl
]

{ #category : 'project specification' }
RwResolvedProjectV2 >> gitUrl: anUrlString [
	self _projectSpecification gitUrl: anUrlString
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

	projectSpecification := RwProjectSpecificationV2 new.
	loadSpecification := RwLoadSpecificationV2 new.
	projectDefinition := RwProjectDefinitionV2 new
]

{ #category : 'actions' }
RwResolvedProjectV2 >> load [
	"load the receiver into the image"

	^ Rowan projectTools loadV2 loadProjectDefinition: self projectDefinition
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
RwResolvedProjectV2 >> packageNamed: aString [
	^ self _projectDefinition packageNamed: aString
]

{ #category : 'project definition' }
RwResolvedProjectV2 >> packageNames [
	^ self _projectDefinition packageNames
]

{ #category : 'accessing' }
RwResolvedProjectV2 >> projectAlias: aString [
	self _loadSpecification projectAlias: aString
]

{ #category : 'accessing' }
RwResolvedProjectV2 >> projectName: aString [
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
	self _projectSpecification projectsHome: aProjectHomeReferenceOrString
]

{ #category : 'actions' }
RwResolvedProjectV2 >> read [
	"refresh the contents of the receiver ... the reciever will match the definitions on disk based on the current load specification"

	"return a project definition set that will contain the project definition along with any dependent project definitions"

	self readComponentNames: self componentNames groupNames: self groupNames
]

{ #category : 'actions' }
RwResolvedProjectV2 >> readComponentNames: componentNames groupNames: groupNames [
	"refresh the contents of the receiver ... the reciever will match the definitions on disk based on the current load specification"

	"return a project definition set that will contain the project definition along with any dependent project definitions"

	| projectSetDefinition |
	projectSetDefinition := Rowan projectTools readV2
		readProjectSetForResolvedProject: self
		withComponentNames: componentNames
		groupNames: groupNames.
	projectDefinition := projectSetDefinition projectNamed: self projectName
]

{ #category : 'actions' }
RwResolvedProjectV2 >> resolve [
	"resolve the projectSpecation (clone remote repo or connect to existing repo on disk) and read 
		project from disk, if project is present on disk"

	self _projectRepository resolve
		ifTrue: [ 
			self _repository checkAndUpdateRepositoryRevision: self.
			self _projectSpecification _checkProjectDirectoryStructure
				ifTrue: [ 
					"update project definition from disk"
					self read ] ]
]
