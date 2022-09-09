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
RwDiskRepositoryDefinitionV2 >> doCommit: message [

	"noop by default"
]

{ #category : 'accessing' }
RwDiskRepositoryDefinitionV2 >> remote [
	^ ''
]

{ #category : 'testing' }
RwDiskRepositoryDefinitionV2 >> repositoryExists [

	^ self repositoryRoot exists
]

{ #category : 'accessing' }
RwDiskRepositoryDefinitionV2 >> repositoryRoot [
	"Root directory of the project. The componentsPath, specsPath, and projectsPath are specified relative to the repository root."

	^ repositoryRoot
		ifNil: [ 
			| fileRef |
			repositoryUrl
				ifNotNil: [ :urlString | 
					| url |
					url := urlString asRwUrl.
					url scheme = 'file'
						ifTrue: [ fileRef := url pathString asFileReference ] ].
			fileRef
				ifNil: [ fileRef := self projectsHome / self name ].
			 self canBeWritten
				ifTrue: [ repositoryRoot := fileRef ]
				ifFalse: [ fileRef ] ]
]

{ #category : 'accessing' }
RwDiskRepositoryDefinitionV2 >> repositoryRoot: pathStringOrReference [
	repositoryRoot := pathStringOrReference asFileReference
]

{ #category : 'actions' }
RwDiskRepositoryDefinitionV2 >> resolveRepository [
	"attach to existing repository structure"

	"answer true if attaching to an existing repository"

	self repositoryRoot exists
		ifFalse: [ self repositoryRoot ensureCreateDirectory ].
	^ true
]

{ #category : 'actions' }
RwDiskRepositoryDefinitionV2 >> updateLoadSpecWithRepositoryRoot: aLoadSpec [
	"preserve the current repositoryRoot in the loadSpec"

	self repositoryUrl
		ifNil: [ aLoadSpec diskUrl: 'file:' , self repositoryRoot pathString ]
		ifNotNil: [ :url | aLoadSpec diskUrl: url ]
]
