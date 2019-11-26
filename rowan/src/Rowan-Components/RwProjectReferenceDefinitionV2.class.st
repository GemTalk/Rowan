Class {
	#name : 'RwProjectReferenceDefinitionV2',
	#superclass : 'RwDefinition',
	#instVars : [
		'components',
		'projectLoadSpecification',
		'projectSpecification'
	],
	#category : 'Rowan-Components'
}

{ #category : 'instance creation' }
RwProjectReferenceDefinitionV2 class >> fromLoadSpecification: aRwProjectLoadSpecification [
	^ self new
		projectLoadSpecification: aRwProjectLoadSpecification;
		yourself
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

	^ self projectLoadSpecification committish
]

{ #category : 'accessing' }
RwProjectReferenceDefinitionV2 >> committish: aString [ 

	self projectLoadSpecification committish: aString.
]

{ #category : 'accessing' }
RwProjectReferenceDefinitionV2 >> componentNames [

	^ self projectLoadSpecification componentNames
]

{ #category : 'accessing' }
RwProjectReferenceDefinitionV2 >> componentNames: anArray [

	^ self projectLoadSpecification componentNames: anArray
]

{ #category : 'accessing' }
RwProjectReferenceDefinitionV2 >> components [
	^ components ifNil: [ components := Dictionary new ]
]

{ #category : 'accessing' }
RwProjectReferenceDefinitionV2 >> components: aComponentDefinitionDictionary [
	^ components := aComponentDefinitionDictionary
]

{ #category : 'accessing' }
RwProjectReferenceDefinitionV2 >> componentsPath [

	^ self projectLoadSpecification componentsPath
]

{ #category : 'accessing' }
RwProjectReferenceDefinitionV2 >> componentsPath: aString [

	^ self projectLoadSpecification componentsPath: aString
]

{ #category : 'accessing' }
RwProjectReferenceDefinitionV2 >> componentsRoot [
	^ self projectLoadSpecification componentsRoot
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

	^self projectSpecification groupNames
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
	^ self projectSpecification packageFormat
]

{ #category : 'accessing' }
RwProjectReferenceDefinitionV2 >> packageFormat: aString [
	self projectLoadSpecification packageFormat: aString
]

{ #category : 'accessing' }
RwProjectReferenceDefinitionV2 >> packageFormatIfAbsent: absentBlock [

	^ self properties at: 'packageFormat' ifAbsent: absentBlock
]

{ #category : 'accessing' }
RwProjectReferenceDefinitionV2 >> packageNameToPlatformPropertiesMap: aDictionary [

	^ self properties at: 'packageNameToPlatformPropertiesMap' put: aDictionary
]

{ #category : 'accessing' }
RwProjectReferenceDefinitionV2 >> packagesPath [

	^ self projectLoadSpecification packagesPath
]

{ #category : 'accessing' }
RwProjectReferenceDefinitionV2 >> packagesPath: aString [

	^ self projectLoadSpecification packagesPath: aString
]

{ #category : 'accessing' }
RwProjectReferenceDefinitionV2 >> packagesRoot [
	^ self projectSpecification packagesRoot
]

{ #category : 'copying' }
RwProjectReferenceDefinitionV2 >> postCopy [
	| componentsCopy |
	super postCopy.
	componentsCopy := self components copy.
	componentsCopy
		keysAndValuesDo: [ :key :value | componentsCopy at: key put: value copy ].
	self components: componentsCopy.
	self projectLoadSpecification: self projectLoadSpecification copy
]

{ #category : 'accessing' }
RwProjectReferenceDefinitionV2 >> projectAlias [
	"project alias is used as the name of the root directory for the project ... necessary if the project is 
		embedded in another project's git repository or you want use a non-default directory on disk"

	^ self projectSpecification projectAlias
]

{ #category : 'accessing' }
RwProjectReferenceDefinitionV2 >> projectAlias: aStringOrNil [
	"project alias is used as the name of the root directory for the project ... necessary if the project is 
		embedded in another project's git repository or you want use a non-default directory on disk"

	self projectLoadSpecification projectAlias: aStringOrNil
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
	"make a copy of the aRwProjectLoadSpecification, because we need to preserve
		isolation of the load spec once it has been associated with the project definition"

	projectLoadSpecification := aRwProjectLoadSpecification copy
]

{ #category : 'accessing' }
RwProjectReferenceDefinitionV2 >> projectName [
	^ self projectSpecification projectName
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

	^ self projectLoadSpecification projectsPath
]

{ #category : 'accessing' }
RwProjectReferenceDefinitionV2 >> projectsPath: aString [

	^ self projectLoadSpecification projectsPath: aString
]

{ #category : 'accessing' }
RwProjectReferenceDefinitionV2 >> projectSpecification [
	^ projectSpecification
]

{ #category : 'accessing' }
RwProjectReferenceDefinitionV2 >> projectSpecification: aRwProjectSpecification [
	projectSpecification := aRwProjectSpecification
]

{ #category : 'accessing' }
RwProjectReferenceDefinitionV2 >> projectsRoot [
	^ self projectLoadSpecification projectsRoot
]

{ #category : 'accessing' }
RwProjectReferenceDefinitionV2 >> projectUrl [

	^ self projectLoadSpecification projectUrl
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
	projectSpecification ifNil: [ ^ nil ].
	^ self projectSpecification repositoryDefinition
]

{ #category : 'accessing' }
RwProjectReferenceDefinitionV2 >> repositoryRoot [
	"Root directory of the project. The componentsPath, repoPath, specsPath, and projectsPath are specified relative to the repository root."

	^ self projectLoadSpecification repositoryRoot
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

{ #category : 'accessing' }
RwProjectReferenceDefinitionV2 >> SHA: aString [

	self committish: aString committishType: 'SHA'
]

{ #category : 'accessing' }
RwProjectReferenceDefinitionV2 >> specsPath [

	^ self projectLoadSpecification specsPath
]

{ #category : 'accessing' }
RwProjectReferenceDefinitionV2 >> specsPath: aString [

	^ self projectLoadSpecification specsPath: aString
]

{ #category : 'accessing' }
RwProjectReferenceDefinitionV2 >> specsRoot [
	^ self projectLoadSpecification specsRoot
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
