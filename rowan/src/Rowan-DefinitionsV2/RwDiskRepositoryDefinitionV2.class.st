Class {
	#name : 'RwDiskRepositoryDefinitionV2',
	#superclass : 'RwAbstractRepositoryDefinitionV2',
	#category : 'Rowan-DefinitionsV2'
}

{ #category : 'instance creation' }
RwDiskRepositoryDefinitionV2 class >> newNamed: repositoryName projectsHome: aFileReference repositoryUrl: aRepositoryUrlString [
	^ self new
		name: repositoryName;
		projectsHome: aFileReference;
		repositoryUrl: aRepositoryUrlString;
		yourself
]

{ #category : 'testing' }
RwDiskRepositoryDefinitionV2 >> canCommit [

	^ false
]

{ #category : 'actions' }
RwDiskRepositoryDefinitionV2 >> checkAndUpdateRepositoryRevision: aRwProjectLoadSpecificationV2 [
	"noop"
]

{ #category : 'actions' }
RwDiskRepositoryDefinitionV2 >> clone [

	"attach to existing cloned disk structure or clone project from remote repository"

	"who wins? resolve or clone"

	self subclassResponsibility: #clone
]

{ #category : 'accessing' }
RwDiskRepositoryDefinitionV2 >> commitId [
	^ ''
]

{ #category : 'accessing' }
RwDiskRepositoryDefinitionV2 >> commitLog: logLimit [

	^ ''
]

{ #category : 'actions' }
RwDiskRepositoryDefinitionV2 >> create [

	self error: 'not yet implemented'
]

{ #category : 'actions' }
RwDiskRepositoryDefinitionV2 >> diskRepositoryRoot: repositoryRootPathString [
	self
		repositoryUrl: 'file:' , repositoryRootPathString;
		repositoryRoot: repositoryRootPathString
]

{ #category : 'actions' }
RwDiskRepositoryDefinitionV2 >> doCommit: message [

	"noop by default"
]

{ #category : 'testing' }
RwDiskRepositoryDefinitionV2 >> repositoryExists [

	^ self repositoryRoot exists
]

{ #category : 'actions' }
RwDiskRepositoryDefinitionV2 >> resolve [
	"attach to existing repository structure"

	"answer true if attaching to an existing repository"

	self repositoryRoot exists
		ifFalse: [ self repositoryRoot ensureCreateDirectory ].
	^ true
]
