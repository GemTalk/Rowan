Class {
	#name : 'RwProjectReferenceDefinitionV2',
	#superclass : 'RwDefinition',
	#instVars : [
		'projectName',
		'projectLoadSpecification',
		'repositoryDefinition'
	],
	#category : 'Rowan-Components'
}

{ #category : 'instance creation' }
RwProjectReferenceDefinitionV2 class >> fromLoadSpecification: aRwProjectLoadSpecification [
	^ self new
		projectName: aRwProjectLoadSpecification projectName;
		projectLoadSpecification: aRwProjectLoadSpecification;
		yourself
]

{ #category : 'private' }
RwProjectReferenceDefinitionV2 >> _createRepositoryFrom: repoDef [ 

	self error: 'not yet implemented'
]

{ #category : 'private' }
RwProjectReferenceDefinitionV2 >> _repositoryDefinitionClass [

	self useGit ifTrue: [ ^ RwGitRepositoryDefinition ].
	^ RwAbstractRepositoryDefinition
]

{ #category : 'private' }
RwProjectReferenceDefinitionV2 >> _repositoryDefinitionForProjectHome: projectHomeFileReferenceOrString [


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
RwProjectReferenceDefinitionV2 >> acceptVisitor: aVisitor [

	^ aVisitor visitProjectReferenceDefinition: self
]

{ #category : 'accessing' }
RwProjectReferenceDefinitionV2 >> branch: aString [

	self committish: aString committishType: 'branch'
]

{ #category : 'properties' }
RwProjectReferenceDefinitionV2 >> canCommit [

	^ self repositoryDefinition canCommit
]

{ #category : 'actions' }
RwProjectReferenceDefinitionV2 >> clone [

	"who wins? resolve or clone"

	^ self repositoryDefinition clone
]

{ #category : 'accessing' }
RwProjectReferenceDefinitionV2 >> comment [

	^ self properties at: 'comment' ifAbsent: [ '' ]
]

{ #category : 'accessing' }
RwProjectReferenceDefinitionV2 >> comment: aString [

	^ self properties at: 'comment' put: aString
]

{ #category : 'accessing' }
RwProjectReferenceDefinitionV2 >> commitLog: logLimit [

	^ self repositoryDefinition commitLog: logLimit
]

{ #category : 'accessing' }
RwProjectReferenceDefinitionV2 >> committish [

	^ self properties at: 'committish' ifAbsent: [ 'master' ]
]

{ #category : 'accessing' }
RwProjectReferenceDefinitionV2 >> committish: aString committishType: committishTypeString [

	self properties at: 'committish' put: aString.
	self properties at: 'committishType' put: committishTypeString.
]

{ #category : 'accessing' }
RwProjectReferenceDefinitionV2 >> committishType [

	^ self properties at: 'committishType'  ifAbsent: [ 'branch' ]
]

{ #category : 'accessing' }
RwProjectReferenceDefinitionV2 >> components [

	^ self properties at: 'components' ifAbsentPut: [ Dictionary new ]
]

{ #category : 'accessing' }
RwProjectReferenceDefinitionV2 >> components: aComponentDefinitionDictionary [

	^ self properties at: 'components' put: aComponentDefinitionDictionary
]

{ #category : 'accessing' }
RwProjectReferenceDefinitionV2 >> configsPath [

	^ self properties at: 'configsPath' ifAbsent: [ 'rowan/components' ]
]

{ #category : 'accessing' }
RwProjectReferenceDefinitionV2 >> configsPath: aString [

	^ self properties at: 'configsPath' put: aString
]

{ #category : 'accessing' }
RwProjectReferenceDefinitionV2 >> configsRoot [

	^self repositoryRoot / self configsPath
]

{ #category : 'accessing' }
RwProjectReferenceDefinitionV2 >> configurationNames [

	^ self properties at: 'configurationNames' ifAbsent: [ { self defaultComponentName } ]
]

{ #category : 'accessing' }
RwProjectReferenceDefinitionV2 >> configurationNames: anArray [

	^ self properties at: 'configurationNames' put: anArray
]

{ #category : 'actions' }
RwProjectReferenceDefinitionV2 >> create [
	"write the project structure to disk, if it doesn't already exist"

	Rowan projectTools create
		createProjectRepository: self.
]

{ #category : 'actions' }
RwProjectReferenceDefinitionV2 >> doCommit: message [

	^ self repositoryDefinition doCommit: message
]

{ #category : 'exporting' }
RwProjectReferenceDefinitionV2 >> exportSpecification [

	| spec |
	spec := self asSpecification.
	spec exportToUrl: 'file:',  self specsRoot pathString, '/'
]

{ #category : 'accessing' }
RwProjectReferenceDefinitionV2 >> gitRoot: aGitRootReferenceOrString [ 

	self repositoryDefinition gitRoot: aGitRootReferenceOrString
]

{ #category : 'accessing' }
RwProjectReferenceDefinitionV2 >> groupNames [

	^self projectLoadSpecification groupNames
]

{ #category : 'accessing' }
RwProjectReferenceDefinitionV2 >> groupNames: aCollection [

	^ self projectLoadSpecification groupNames: aCollection asArray
]

{ #category : 'temporary compat' }
RwProjectReferenceDefinitionV2 >> imageSpec [
self deprecated: 'temporary method ... simulating RwSpecification api'.
	^self
]

{ #category : 'accessing' }
RwProjectReferenceDefinitionV2 >> key [

	^ self projectName
]

{ #category : 'accessing' }
RwProjectReferenceDefinitionV2 >> loadedCommitId [

	^ self properties at: 'loadedCommitId' ifAbsent: [ '' ]
]

{ #category : 'accessing' }
RwProjectReferenceDefinitionV2 >> loadedCommitId:  aCommitId [

	^ self properties at: 'loadedCommitId' put: aCommitId
]

{ #category : 'accessing' }
RwProjectReferenceDefinitionV2 >> loadedConfigurationNames [

	^ self components isEmpty
		ifTrue: [ self configurationNames ]
		ifFalse: [ (self components values select: [:each | each isIndependentlyLoadable ]) collect: [:each | each name ] ]
]

{ #category : 'accessing' }
RwProjectReferenceDefinitionV2 >> loadedConfigurationNames: anArray [

	"https://github.com/GemTalk/Rowan/issues/308"

	"eventually this method will be completely removed/deprecated"

	^ self configurationNames: anArray
]

{ #category : 'accessing' }
RwProjectReferenceDefinitionV2 >> loadedGroupNames [

	^ self groupNames
]

{ #category : 'accessing' }
RwProjectReferenceDefinitionV2 >> loadedGroupNames: anArray [

	^ self groupNames: anArray
]

{ #category : 'accessing' }
RwProjectReferenceDefinitionV2 >> packageConvention [

	^ self properties at: 'packageConvention' ifAbsent: [ 'RowanHybrid' ]
]

{ #category : 'accessing' }
RwProjectReferenceDefinitionV2 >> packageConvention: aString [

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
RwProjectReferenceDefinitionV2 >> packageFormat [

	^ self packageFormatIfAbsent: [ 'tonel' ]
]

{ #category : 'accessing' }
RwProjectReferenceDefinitionV2 >> packageFormat: aString [

	(#( 'tonel' 'filetree' ) includes: aString)
		ifFalse: [ self error: 'Unknown package format ', aString printString, '. Should be one of: tonel, or filetree' ].
	^ self properties at: 'packageFormat' put: aString
]

{ #category : 'accessing' }
RwProjectReferenceDefinitionV2 >> packageFormatIfAbsent: absentBlock [

	^ self properties at: 'packageFormat' ifAbsent: absentBlock
]

{ #category : 'accessing' }
RwProjectReferenceDefinitionV2 >> packagesPath [

	^ self properties at: 'packagesPath' ifAbsent: [ 'rowan/src' ]
]

{ #category : 'accessing' }
RwProjectReferenceDefinitionV2 >> packagesPath: aString [

	^ self properties at: 'packagesPath' put: aString
]

{ #category : 'accessing' }
RwProjectReferenceDefinitionV2 >> packagesRoot [

	^self repositoryRoot / self packagesPath
]

{ #category : 'copying' }
RwProjectReferenceDefinitionV2 >> postCopy [

	| components |
	super postCopy.
	components := self components copy.
	components keysAndValuesDo: [:key :value | components at: key put: value copy ].
	self components: components
]

{ #category : 'accessing' }
RwProjectReferenceDefinitionV2 >> projectAlias [
	"project alias is used as the name of the root directory for the project ... necessary if the project is 
		embedded in another project's git repository or you want use a non-default directory on disk"

	^ self properties at: 'projectAlias' ifAbsent: [ self projectName ]
]

{ #category : 'accessing' }
RwProjectReferenceDefinitionV2 >> projectAlias: aStringOrNil [
	"project alias is used as the name of the root directory for the project ... necessary if the project is 
		embedded in another project's git repository or you want use a non-default directory on disk"

	aStringOrNil
		ifNil: [ ^ self properties removeKey: 'projectAlias' ifAbsent: [  ] ].
	^ self properties at: 'projectAlias' put: aStringOrNil
]

{ #category : 'accessing' }
RwProjectReferenceDefinitionV2 >> projectHome [

	^ self properties at: 'projectHome' ifAbsent: [ FileLocator rowanProjectsHome ]
]

{ #category : 'accessing' }
RwProjectReferenceDefinitionV2 >> projectHome: projectHomeFileReferenceOrString [

	self repositoryDefinition: nil. "changing project home invalidates the current repository definition"
	projectHomeFileReferenceOrString
		ifNil: [ ^ self properties removeKey: 'projectHome' ifAbsent: [] ].
	self properties at: 'projectHome' put: projectHomeFileReferenceOrString asFileReference.
]

{ #category : 'accessing' }
RwProjectReferenceDefinitionV2 >> projectLoadSpecification [
	^ projectLoadSpecification
]

{ #category : 'accessing' }
RwProjectReferenceDefinitionV2 >> projectLoadSpecification: aRwProjectLoadSpecification [
	projectLoadSpecification := aRwProjectLoadSpecification
]

{ #category : 'accessing' }
RwProjectReferenceDefinitionV2 >> projectName [
	^ projectName
]

{ #category : 'accessing' }
RwProjectReferenceDefinitionV2 >> projectName: aString [
	self repositoryDefinition: nil.	"changing project name invalidates the current repository definition"
	projectName := aString
]

{ #category : 'accessing' }
RwProjectReferenceDefinitionV2 >> projectNames [

	^ self properties at: 'projectNames' ifAbsent: [ #() ]
]

{ #category : 'accessing' }
RwProjectReferenceDefinitionV2 >> projectNames: anArray [

	^ self properties at: 'projectNames' put: anArray
]

{ #category : 'accessing' }
RwProjectReferenceDefinitionV2 >> projectsPath [

	^ self properties at: 'projectsPath' ifAbsent: [ 'rowan/projects' ]
]

{ #category : 'accessing' }
RwProjectReferenceDefinitionV2 >> projectsPath: aString [

	^ self properties at: 'projectsPath' put: aString
]

{ #category : 'accessing' }
RwProjectReferenceDefinitionV2 >> projectsRoot [

	^self repositoryRoot / self projectsPath
]

{ #category : 'accessing' }
RwProjectReferenceDefinitionV2 >> projectUrl [

	^ self properties at: 'projectUrl' ifAbsent: [ '' ]
]

{ #category : 'accessing' }
RwProjectReferenceDefinitionV2 >> projectUrl: aString [

	^ self properties at: 'projectUrl' put: aString
]

{ #category : 'accessing' }
RwProjectReferenceDefinitionV2 >> remote [

	^ self properties at: 'remote' ifAbsent: [ 'origin' ]
]

{ #category : 'accessing' }
RwProjectReferenceDefinitionV2 >> remoteUrl [

	^ self repositoryDefinition remoteUrl
]

{ #category : 'accessing' }
RwProjectReferenceDefinitionV2 >> remoteUrl: aRemoteUrl [

	self repositoryDefinition remoteUrl: aRemoteUrl
]

{ #category : 'accessing' }
RwProjectReferenceDefinitionV2 >> repositoryCommitId [

	^ self repositoryDefinition repositoryCommitId
]

{ #category : 'accessing' }
RwProjectReferenceDefinitionV2 >> repositoryDefinition [

	^ repositoryDefinition ifNil:  [ repositoryDefinition := self _repositoryDefinitionForProjectHome: self projectHome ]
]

{ #category : 'accessing' }
RwProjectReferenceDefinitionV2 >> repositoryDefinition: aRepositoryDefinition [

	repositoryDefinition := aRepositoryDefinition
]

{ #category : 'accessing' }
RwProjectReferenceDefinitionV2 >> repositoryRoot [
	"Root directory of the project. The configsPath, repoPath, specsPath, and projectsPath are specified relative to the repository root."

	^self projectHome / self projectName
]

{ #category : 'accessing' }
RwProjectReferenceDefinitionV2 >> repositoryRoot: aFileReferenceOrPath [
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
RwProjectReferenceDefinitionV2 >> repositoryRootPath [

	^ self repositoryRoot pathString
]

{ #category : 'temporary compat' }
RwProjectReferenceDefinitionV2 >> repoSpec [

self deprecated: 'temporary method ... simulating RwSpecification api'.
	^self
]

{ #category : 'actions' }
RwProjectReferenceDefinitionV2 >> resolve [

	"who wins? resolve or clone"

	^ self repositoryDefinition resolve
]

{ #category : 'accessing' }
RwProjectReferenceDefinitionV2 >> SHA: aString [

	self committish: aString committishType: 'SHA'
]

{ #category : 'accessing' }
RwProjectReferenceDefinitionV2 >> specsPath [

	^ self properties at: 'specsPath' ifAbsent: [ 'rowan/specs' ]
]

{ #category : 'accessing' }
RwProjectReferenceDefinitionV2 >> specsPath: aString [

	^ self properties at: 'specsPath' put: aString
]

{ #category : 'accessing' }
RwProjectReferenceDefinitionV2 >> specsRoot [

	^self repositoryRoot / self specsPath
]

{ #category : 'accessing' }
RwProjectReferenceDefinitionV2 >> tag: aString [

	self committish: aString committishType: 'tag'
]

{ #category : 'loading' }
RwProjectReferenceDefinitionV2 >> updateLoadedCommitId [

	self loadedCommitId: self repositoryDefinition repositoryCommitId
]

{ #category : 'temporary compat' }
RwProjectReferenceDefinitionV2 >> updateLoadedCommitIdForTool: aRwTool [
self deprecated: 'temporary method ... simulating RwSpecification api'.
  ^ aRwTool doGitUpdateLoadedCommitId
]

{ #category : 'accessing' }
RwProjectReferenceDefinitionV2 >> useGit [

	^ self properties at: 'useGit' ifAbsent: [ false ]
]

{ #category : 'accessing' }
RwProjectReferenceDefinitionV2 >> useGit: aBool [

	^ self properties at: 'useGit' put: aBool
]

{ #category : 'actions' }
RwProjectReferenceDefinitionV2 >> validateRepository [
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
