Class {
	#name : 'RwDiskRepositoryDefinitionV2',
	#superclass : 'RwDefinition',
	#instVars : [
		'projectsHome',
		'repositoryRoot',
		'repositoryUrl'
	],
	#category : 'Rowan-Components'
}

{ #category : 'instance creation' }
RwDiskRepositoryDefinitionV2 class >> newNamed: repositoryName projectsHome: aFileReference repositoryUrl: aRepositoryUrlString [
	^ self new
		name: repositoryName;
		projectsHome: aFileReference;
		repositoryUrl: aRepositoryUrlString;
		yourself
]

{ #category : 'private' }
RwDiskRepositoryDefinitionV2 >> _fileSystem [
	^ FileSystem disk
]

{ #category : 'comparing' }
RwDiskRepositoryDefinitionV2 >> = anObject [
	^ self projectsHome = anObject projectsHome
		and: [ 
			self repositoryRoot = anObject repositoryRoot
				and: [ self repositoryUrl = anObject repositoryUrl ] ]
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

{ #category : 'comparing' }
RwDiskRepositoryDefinitionV2 >> hash [
	| hashValue |
	hashValue := self projectsHome hash.
	hashValue := hashValue bitXor: self repositoryRoot hash.
	hashValue := hashValue bitXor: self repositoryUrl hash.
	^ hashValue
]

{ #category : 'accessing' }
RwDiskRepositoryDefinitionV2 >> key [

	^properties at: 'name' ifAbsent: [nil]
]

{ #category : 'accessing' }
RwDiskRepositoryDefinitionV2 >> name [

	^ self key
]

{ #category : 'accessing' }
RwDiskRepositoryDefinitionV2 >> name: aString [ 

	properties at: 'name' put: aString
]

{ #category : 'accessing' }
RwDiskRepositoryDefinitionV2 >> projectsHome [
	^ projectsHome
]

{ #category : 'accessing' }
RwDiskRepositoryDefinitionV2 >> projectsHome: aFileReference [
	projectsHome := aFileReference
]

{ #category : 'testing' }
RwDiskRepositoryDefinitionV2 >> repositoryExists [

	^ self repositoryRoot exists
]

{ #category : 'accessing' }
RwDiskRepositoryDefinitionV2 >> repositoryRoot [
	"Root directory of the project. The configsPath, repoPath, specsPath, and projectsPath are specified relative to the repository root."

	^ repositoryRoot
		ifNil: [ 
			repositoryUrl
				ifNotNil: [ :urlString | 
					| url |
					url := urlString asRwUrl.
					url scheme = 'file'
						ifTrue: [ ^ repositoryRoot := url pathString asFileReference ] ].
			repositoryRoot := self projectsHome / self name ]
]

{ #category : 'accessing' }
RwDiskRepositoryDefinitionV2 >> repositoryRoot: pathStringOrReference [
	repositoryRoot := pathStringOrReference asFileReference
]

{ #category : 'accessing' }
RwDiskRepositoryDefinitionV2 >> repositoryUrl [
	^ repositoryUrl
]

{ #category : 'accessing' }
RwDiskRepositoryDefinitionV2 >> repositoryUrl: anUrlString [
	repositoryUrl := anUrlString
]

{ #category : 'actions' }
RwDiskRepositoryDefinitionV2 >> resolve [
	"attach to existing repository structure or create"

	"answer true if attaching to an existing repository"

	^ self repositoryRoot exists
		ifTrue: [ true ]
		ifFalse: [ 
			self repositoryRoot ensureCreateDirectory.
			false ]
]

{ #category : 'accessing' }
RwDiskRepositoryDefinitionV2 >> revision [

	^ ''
]
