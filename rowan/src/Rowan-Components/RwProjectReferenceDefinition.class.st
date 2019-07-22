Class {
	#name : 'RwProjectReferenceDefinition',
	#superclass : 'RwDefinition',
	#category : 'Rowan-Components'
}

{ #category : 'instance creation' }
RwProjectReferenceDefinition class >> newForSpecification: aRwSpecification [

	^ self
		newNamed: aRwSpecification projectName
			forSpecification: aRwSpecification 
			projectHome: nil
]

{ #category : 'instance creation' }
RwProjectReferenceDefinition class >> newForSpecification: aRwSpecification projectHome: projectHomeFileReferenceOrString [

	^ self 
		newNamed: aRwSpecification specName 
			forSpecification: aRwSpecification 
			projectHome: projectHomeFileReferenceOrString
]

{ #category : 'instance creation' }
RwProjectReferenceDefinition class >> newNamed: projectName forSpecification: aRwSpecification [

	^ self
		newNamed: projectName 
			forSpecification: aRwSpecification 
			projectHome: nil
]

{ #category : 'instance creation' }
RwProjectReferenceDefinition class >> newNamed: projectName forSpecification: aRwSpecification projectHome: projectHomeFileReferenceOrString [

	^ (self
		projectName: projectName
			configurationNames: aRwSpecification defaultConfigurationNames
			groupNames: aRwSpecification defaultGroupNames
			defaultComponentName: aRwSpecification defaultComponentName
			useGit: aRwSpecification useGit
			projectUrl: aRwSpecification projectUrl
			comment: aRwSpecification comment
			projectHome: projectHomeFileReferenceOrString
			committish: aRwSpecification repoSpec committish
			committishType: aRwSpecification repoSpec committishType
			configsPath: aRwSpecification configsPath
			packagesPath: aRwSpecification repoPath
			projectsPath: aRwSpecification projectsPath
			specsPath: aRwSpecification specsPath)
				projectAlias: aRwSpecification projectAlias;
				yourself
]

{ #category : 'instance creation' }
RwProjectReferenceDefinition class >> projectName: projectName configurationNames: configurationNames groupNames: groupNames defaultComponentName: defaultComponentName useGit: useGit projectUrl: projectUrl comment: comment projectHome: projectHomeFileReferenceOrString committish: committish committishType: committishType configsPath: configsPath packagesPath: packagesPath projectsPath: projectsPath specsPath: specsPath [

	^ self new
		projectName: projectName;
		configurationNames: configurationNames;
		defaultComponentName: defaultComponentName;
		groupNames: groupNames;
		projectUrl: projectUrl;
		useGit: useGit;
		comment: comment;
		projectHome: projectHomeFileReferenceOrString;
		committish: committish committishType: committishType;
		configsPath: configsPath;
		packagesPath: packagesPath;
		projectsPath: projectsPath;
		specsPath: specsPath;
		yourself
]

{ #category : 'instance creation' }
RwProjectReferenceDefinition class >> projectName: projectName configurationNames: configurationNames groupNames: groupNames defaultComponentName: defaultComponentName useGit: useGit projectUrl: projectUrl projectHome: projectHomeFileReferenceOrString committish: committish committishType: committishType comment: comment [

	^ self new
		projectName: projectName;
		configurationNames: configurationNames;
		groupNames: groupNames;
		 defaultComponentName: defaultComponentName;
		useGit: useGit;
		projectUrl: projectUrl;
		projectHome: projectHomeFileReferenceOrString;
		comment: comment;
		committish: committish committishType: committishType;
		yourself
]

{ #category : 'instance creation' }
RwProjectReferenceDefinition class >> projectName: projectName configurationNames: configurationNames groupNames: groupNames useGit: useGit projectUrl: projectUrl comment: comment [

	^ self new
		projectName: projectName;
		configurationNames: configurationNames;
		groupNames: groupNames;
		useGit: useGit;
		projectUrl: projectUrl;
		comment: comment;
		yourself
]

{ #category : 'instance creation' }
RwProjectReferenceDefinition class >> projectName: projectName configurationNames: configurationNames groupNames: groupNames useGit: useGit projectUrl: projectUrl committish: committish committishType: committishType comment: comment [

	^ self new
		projectName: projectName;
		configurationNames: configurationNames;
		groupNames: groupNames;
		useGit: useGit;
		projectUrl: projectUrl;
		comment: comment;
		committish: committish committishType: committishType;
		yourself
]

{ #category : 'instance creation' }
RwProjectReferenceDefinition class >> projectName: projectName configurationNames: configurationNames groupNames: groupNames useGit: useGit projectUrl: projectUrl projectHome: projectHomeFileReferenceOrString committish: committish committishType: committishType comment: comment [

	^ self new
		projectName: projectName;
		configurationNames: configurationNames;
		groupNames: groupNames;
		useGit: useGit;
		projectUrl: projectUrl;
		projectHome: projectHomeFileReferenceOrString;
		comment: comment;
		committish: committish committishType: committishType;
		yourself
]

{ #category : 'instance creation' }
RwProjectReferenceDefinition class >> projectName: projectName useGit: useGit [

	^ self new
		projectName: projectName;
		useGit: useGit;
		yourself
]

{ #category : 'private' }
RwProjectReferenceDefinition >> _createRepositoryFrom: repoDef [ 

	self error: 'not yet implemented'
]

{ #category : 'private' }
RwProjectReferenceDefinition >> _repositoryDefinitionClass [

	self useGit ifTrue: [ ^ RwGitRepositoryDefinition ].
	^ RwAbstractRepositoryDefinition
]

{ #category : 'private' }
RwProjectReferenceDefinition >> _repositoryDefinitionForProjectHome: projectHomeFileReferenceOrString [


	| newProjectRepository |
	newProjectRepository := self _repositoryDefinitionClass
		newNamed: self projectAlias
			repositoryRoot: self repositoryRoot	"not a good idea to have repository root stored in two spots"
			projectUrl: self projectUrl
			committish: self committish 
			committishType: self committishType.
	^ newProjectRepository
]

{ #category : 'visiting' }
RwProjectReferenceDefinition >> acceptVisitor: aVisitor [

	^ aVisitor visitProjectReferenceDefinition: self
]

{ #category : 'accessing' }
RwProjectReferenceDefinition >> branch: aString [

	self committish: aString committishType: 'branch'
]

{ #category : 'properties' }
RwProjectReferenceDefinition >> canCommit [

	^ self repositoryDefinition canCommit
]

{ #category : 'actions' }
RwProjectReferenceDefinition >> clone [

	"who wins? resolve or clone"

	^ self repositoryDefinition clone
]

{ #category : 'accessing' }
RwProjectReferenceDefinition >> comment [

	^ self properties at: 'comment' ifAbsent: [ '' ]
]

{ #category : 'accessing' }
RwProjectReferenceDefinition >> comment: aString [

	^ self properties at: 'comment' put: aString
]

{ #category : 'accessing' }
RwProjectReferenceDefinition >> commitLog: logLimit [

	^ self repositoryDefinition commitLog: logLimit
]

{ #category : 'accessing' }
RwProjectReferenceDefinition >> committish [

	^ self properties at: 'committish' ifAbsent: [ 'master' ]
]

{ #category : 'accessing' }
RwProjectReferenceDefinition >> committish: aString committishType: committishTypeString [

	self properties at: 'committish' put: aString.
	self properties at: 'committishType' put: committishTypeString.
]

{ #category : 'accessing' }
RwProjectReferenceDefinition >> committishType [

	^ self properties at: 'committishType'  ifAbsent: [ 'branch' ]
]

{ #category : 'accessing' }
RwProjectReferenceDefinition >> components [

	^ self properties at: 'components' ifAbsentPut: [ Dictionary new ]
]

{ #category : 'accessing' }
RwProjectReferenceDefinition >> components: aComponentDefinitionDictionary [

	^ self properties at: 'components' put: aComponentDefinitionDictionary
]

{ #category : 'accessing' }
RwProjectReferenceDefinition >> configsPath [

	^ self properties at: 'configsPath' ifAbsent: [ 'rowan/components' ]
]

{ #category : 'accessing' }
RwProjectReferenceDefinition >> configsPath: aString [

	^ self properties at: 'configsPath' put: aString
]

{ #category : 'accessing' }
RwProjectReferenceDefinition >> configsRoot [

	^self repositoryRoot / self configsPath
]

{ #category : 'accessing' }
RwProjectReferenceDefinition >> configurationNames [

	^ self properties at: 'configurationNames' ifAbsent: [ { self defaultComponentName } ]
]

{ #category : 'accessing' }
RwProjectReferenceDefinition >> configurationNames: anArray [

	^ self properties at: 'configurationNames' put: anArray
]

{ #category : 'actions' }
RwProjectReferenceDefinition >> create [
	"write the project structure to disk, if it doesn't already exist"

	Rowan projectTools create
		createProjectRepository: self.
]

{ #category : 'accessing' }
RwProjectReferenceDefinition >> defaultComponentName [

	^ self properties at: 'defaultComponentName' ifAbsent: [ 'Core' ]
]

{ #category : 'accessing' }
RwProjectReferenceDefinition >> defaultComponentName: aString [

	^ self properties at: 'defaultComponentName' put: aString
]

{ #category : 'accessing' }
RwProjectReferenceDefinition >> defaultGroupName [

	^ self properties at: 'defaultGroupName' ifAbsent: [ 'core' ]
]

{ #category : 'accessing' }
RwProjectReferenceDefinition >> defaultGroupName: aString [

	^ self properties at: 'defaultGroupName' put: aString
]

{ #category : 'actions' }
RwProjectReferenceDefinition >> doCommit: message [

	^ self repositoryDefinition doCommit: message
]

{ #category : 'exporting' }
RwProjectReferenceDefinition >> exportSpecification [

	| spec |
	spec := self asSpecification.
	spec exportToUrl: 'file:',  self specsRoot pathString, '/'
]

{ #category : 'accessing' }
RwProjectReferenceDefinition >> gitRoot: aGitRootReferenceOrString [ 

	self repositoryDefinition gitRoot: aGitRootReferenceOrString
]

{ #category : 'accessing' }
RwProjectReferenceDefinition >> groupNames [

	| gns |
	gns := self properties at: 'groupNames' ifAbsent: [ {} ].
	^ gns isEmpty
		ifTrue: [ { self defaultGroupName } ]
		ifFalse: [ gns ]
]

{ #category : 'accessing' }
RwProjectReferenceDefinition >> groupNames: aSet [

	^ self properties at: 'groupNames' put: aSet asSet
]

{ #category : 'temporary compat' }
RwProjectReferenceDefinition >> imageSpec [
self deprecated: 'temporary method ... simulating RwSpecification api'.
	^self
]

{ #category : 'accessing' }
RwProjectReferenceDefinition >> key [

	^ self projectName
]

{ #category : 'accessing' }
RwProjectReferenceDefinition >> loadedCommitId [

	^ self properties at: 'loadedCommitId' ifAbsent: [ '' ]
]

{ #category : 'accessing' }
RwProjectReferenceDefinition >> loadedCommitId:  aCommitId [

	^ self properties at: 'loadedCommitId' put: aCommitId
]

{ #category : 'accessing' }
RwProjectReferenceDefinition >> loadedConfigurationNames [

	^ self components isEmpty
		ifTrue: [ self configurationNames ]
		ifFalse: [ (self components values select: [:each | each isIndependentlyLoadable ]) collect: [:each | each name ] ]
]

{ #category : 'accessing' }
RwProjectReferenceDefinition >> loadedConfigurationNames: anArray [

	"https://github.com/GemTalk/Rowan/issues/308"

	"eventually this method will be completely removed/deprecated"

	^ self configurationNames: anArray
]

{ #category : 'accessing' }
RwProjectReferenceDefinition >> loadedGroupNames [

	^ self groupNames
]

{ #category : 'accessing' }
RwProjectReferenceDefinition >> loadedGroupNames: anArray [

	^ self groupNames: anArray
]

{ #category : 'accessing' }
RwProjectReferenceDefinition >> packageConvention [

	^ self properties at: 'packageConvention' ifAbsent: [ 'RowanHybrid' ]
]

{ #category : 'accessing' }
RwProjectReferenceDefinition >> packageConvention: aString [

	"
		RowanHybrid	- [default] Class category is package name, method protocol with leading $* is case insensitive package name
		Monticello		- Class category is package name, method protocol with leading $* begins with case insensitive package name
		Rowan			- Class category and method protocol are not overloaded with packaging information
	"

	(#( 'RowanHybrid' 'Monticello' 'Rowan' ) includes: aString)
		ifFalse: [ self error: 'Unknown package convention ', aString printString, '. Should be one of: RowanHybrid, Monticello, or Rowan' ].
	^ self properties at: 'packageConvention' put: aString
]

{ #category : 'accessing' }
RwProjectReferenceDefinition >> packageFormat [

	^ self packageFormatIfAbsent: [ 'tonel' ]
]

{ #category : 'accessing' }
RwProjectReferenceDefinition >> packageFormat: aString [

	(#( 'tonel' 'filetree' ) includes: aString)
		ifFalse: [ self error: 'Unknown package format ', aString printString, '. Should be one of: tonel, or filetree' ].
	^ self properties at: 'packageFormat' put: aString
]

{ #category : 'accessing' }
RwProjectReferenceDefinition >> packageFormatIfAbsent: absentBlock [

	^ self properties at: 'packageFormat' ifAbsent: absentBlock
]

{ #category : 'accessing' }
RwProjectReferenceDefinition >> packagesPath [

	^ self properties at: 'packagesPath' ifAbsent: [ 'rowan/src' ]
]

{ #category : 'accessing' }
RwProjectReferenceDefinition >> packagesPath: aString [

	^ self properties at: 'packagesPath' put: aString
]

{ #category : 'accessing' }
RwProjectReferenceDefinition >> packagesRoot [

	^self repositoryRoot / self packagesPath
]

{ #category : 'copying' }
RwProjectReferenceDefinition >> postCopy [

	| components |
	super postCopy.
	components := self components copy.
	components keysAndValuesDo: [:key :value | components at: key put: value copy ].
	self components: components
]

{ #category : 'accessing' }
RwProjectReferenceDefinition >> projectAlias [

	^ self properties at: 'projectAlias' ifAbsent: [ self projectName ]
]

{ #category : 'accessing' }
RwProjectReferenceDefinition >> projectAlias: aStringOrNil [

	aStringOrNil ifNil: [ ^ self properties removeKey: 'projectAlias' ifAbsent: [] ].
	^ self properties at: 'projectAlias' put: aStringOrNil
]

{ #category : 'accessing' }
RwProjectReferenceDefinition >> projectHome [

	^ self properties at: 'projectHome' ifAbsent: [ FileLocator rowanProjectsHome ]
]

{ #category : 'accessing' }
RwProjectReferenceDefinition >> projectHome: projectHomeFileReferenceOrString [

	self repositoryDefinition: nil. "changing project home invalidates the current repository definition"
	projectHomeFileReferenceOrString
		ifNil: [ ^ self properties removeKey: 'projectHome' ifAbsent: [] ].
	self properties at: 'projectHome' put: projectHomeFileReferenceOrString asFileReference.
]

{ #category : 'accessing' }
RwProjectReferenceDefinition >> projectName [

	^ self properties at: 'projectName' ifAbsent: []
]

{ #category : 'accessing' }
RwProjectReferenceDefinition >> projectName: aString [

	self repositoryDefinition: nil. "changing project name invalidates the current repository definition"
	^ self properties at: 'projectName' put: aString
]

{ #category : 'accessing' }
RwProjectReferenceDefinition >> projectNames [

	^ self properties at: 'projectNames' ifAbsent: [ #() ]
]

{ #category : 'accessing' }
RwProjectReferenceDefinition >> projectNames: anArray [

	^ self properties at: 'projectNames' put: anArray
]

{ #category : 'accessing' }
RwProjectReferenceDefinition >> projectsPath [

	^ self properties at: 'projectsPath' ifAbsent: [ 'rowan/projects' ]
]

{ #category : 'accessing' }
RwProjectReferenceDefinition >> projectsPath: aString [

	^ self properties at: 'projectsPath' put: aString
]

{ #category : 'accessing' }
RwProjectReferenceDefinition >> projectsRoot [

	^self repositoryRoot / self projectsPath
]

{ #category : 'accessing' }
RwProjectReferenceDefinition >> projectUrl [

	^ self properties at: 'projectUrl' ifAbsent: [ '' ]
]

{ #category : 'accessing' }
RwProjectReferenceDefinition >> projectUrl: aString [

	^ self properties at: 'projectUrl' put: aString
]

{ #category : 'accessing' }
RwProjectReferenceDefinition >> remoteUrl [

	^ self repositoryDefinition remoteUrl
]

{ #category : 'accessing' }
RwProjectReferenceDefinition >> remoteUrl: aRemoteUrl [

	self repositoryDefinition remoteUrl: aRemoteUrl
]

{ #category : 'accessing' }
RwProjectReferenceDefinition >> repositoryCommitId [

	^ self repositoryDefinition repositoryCommitId
]

{ #category : 'accessing' }
RwProjectReferenceDefinition >> repositoryDefinition [

	^ self properties at: 'repositoryDefinition' ifAbsentPut: [ self _repositoryDefinitionForProjectHome: self projectHome ]
]

{ #category : 'accessing' }
RwProjectReferenceDefinition >> repositoryDefinition: aRepositoryDefinition [

	aRepositoryDefinition ifNil: [ ^ self properties removeKey:  'repositoryDefinition' ifAbsent: [] ].
	^ self properties at: 'repositoryDefinition' put: aRepositoryDefinition
]

{ #category : 'accessing' }
RwProjectReferenceDefinition >> repositoryRoot [
	"Root directory of the project. The configsPath, repoPath, specsPath, and projectsPath are specified relative to the repository root."

	^self projectHome / self projectName
]

{ #category : 'accessing' }
RwProjectReferenceDefinition >> repositoryRoot: aFileReferenceOrPath [
	"Root directory of the project. The configsPath, repoPath, specsPath, and projectsPath are specified relative to the repository root."

	| fileRef projectDirName |
	fileRef := aFileReferenceOrPath asFileReference.
	self projectHome: fileRef parent.	 "changing project home invalidates the current repository definition"
	"the project name should match the name of the directory ... use the alias if the names don't match"
	projectDirName := fileRef basename.
	self projectName
		ifNotNil: [:nm | 
			nm = projectDirName
				ifTrue: [
					"we're done"
					^ self ].
			"project name does not equal name project directory"
			self projectAlias = self projectName
				ifTrue: [
					"move the old name to the alias"
					self projectAlias: self projectName ] ].
	"set project name to projectDirName "
	self projectName: projectDirName
]

{ #category : 'temporary compat' }
RwProjectReferenceDefinition >> repositoryRootPath [

	^ self repositoryRoot pathString
]

{ #category : 'temporary compat' }
RwProjectReferenceDefinition >> repoSpec [

self deprecated: 'temporary method ... simulating RwSpecification api'.
	^self
]

{ #category : 'actions' }
RwProjectReferenceDefinition >> resolve [

	"who wins? resolve or clone"

	^ self repositoryDefinition resolve
]

{ #category : 'accessing' }
RwProjectReferenceDefinition >> SHA: aString [

	self committish: aString committishType: 'SHA'
]

{ #category : 'accessing' }
RwProjectReferenceDefinition >> specsPath [

	^ self properties at: 'specsPath' ifAbsent: [ 'rowan/specs' ]
]

{ #category : 'accessing' }
RwProjectReferenceDefinition >> specsPath: aString [

	^ self properties at: 'specsPath' put: aString
]

{ #category : 'accessing' }
RwProjectReferenceDefinition >> specsRoot [

	^self repositoryRoot / self specsPath
]

{ #category : 'accessing' }
RwProjectReferenceDefinition >> tag: aString [

	self committish: aString committishType: 'tag'
]

{ #category : 'loading' }
RwProjectReferenceDefinition >> updateLoadedCommitId [

	self loadedCommitId: self repositoryDefinition commitId
]

{ #category : 'temporary compat' }
RwProjectReferenceDefinition >> updateLoadedCommitIdForTool: aRwTool [
self deprecated: 'temporary method ... simulating RwSpecification api'.
  ^ aRwTool doGitUpdateLoadedCommitId
]

{ #category : 'accessing' }
RwProjectReferenceDefinition >> useGit [

	^ self properties at: 'useGit' ifAbsent: [ false ]
]

{ #category : 'accessing' }
RwProjectReferenceDefinition >> useGit: aBool [

	^ self properties at: 'useGit' put: aBool
]

{ #category : 'actions' }
RwProjectReferenceDefinition >> validateRepository [
	"cofirm that the repository appears to be complete relative to this project"

	{
		'configs' -> self configsRoot. 
		'packages' -> self packagesRoot. 
		'specs' -> self specsRoot. 
		'projects' -> self projectsRoot
	}
		do: [ :assoc | 
			(assoc value) exists 
				ifFalse: [ self error: 'The repository at ', self repositoryRootPath printString, ' is missing the ', (assoc key) printString, ' directory.'  ] ].
]
