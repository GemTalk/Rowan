Class {
	#name : 'RwGitRepositoryDefinition',
	#superclass : 'RwDiskRepositoryDefinition',
	#category : 'Rowan-Components'
}

{ #category : 'instance creation' }
RwGitRepositoryDefinition class >> newNamed: repositoryName forSpecification: aRwSpecification repositoryRoot: repoRoot [

	^ (super newNamed: repositoryName forSpecification: aRwSpecification repositoryRoot: repoRoot)
		committish: aRwSpecification repoSpec committish committishType: aRwSpecification repoSpec committishType;
		remoteUrl: aRwSpecification remoteUrl;
		yourself
]

{ #category : 'instance creation' }
RwGitRepositoryDefinition class >> newNamed: repositoryName repositoryRoot: repoRoot projectUrl: anUrlString remoteUrl: aRemoreUrlStrng committish: committish committishType:committishType [

	^ self new
		name: repositoryName;
		repositoryRoot: repoRoot;
		projectUrl: anUrlString;
		remoteUrl: aRemoreUrlStrng;
		committish: committish committishType:committishType;
		yourself
]

{ #category : 'private' }
RwGitRepositoryDefinition >> _createRemoteUrl [

	| projectUrl remoteUrl segments |
	projectUrl := RwUrl fromString: self projectUrl.
	remoteUrl := 'git@' , projectUrl authority , ':'.
	segments := projectUrl segments.
	remoteUrl := segments size = 1
		ifTrue: [ remoteUrl , (segments at: 1) , '.git ' ]
		ifFalse: [ remoteUrl , (segments at: 1) , '/' , (projectUrl segments at: 2) , '.git ' ].
	^ remoteUrl
]

{ #category : 'accessing' }
RwGitRepositoryDefinition >> commitId [
self error: 'use a git command to extract commitId'
]

{ #category : 'accessing' }
RwGitRepositoryDefinition >> commitId: aString [

	self shouldNotImplement: #commitId
]

{ #category : 'accessing' }
RwGitRepositoryDefinition >> remoteUrl [

	^ self properties at: 'remoteUrl' ifAbsentPut: [ self _createRemoteUrl ]
]

{ #category : 'accessing' }
RwGitRepositoryDefinition >> remoteUrl: aRemoteUrlString [

	^ self properties at: 'remoteUrl' put: aRemoteUrlString
]

{ #category : 'actions' }
RwGitRepositoryDefinition >> resolve [

	"attach to existing git repository structure, clone, or create"

	Rowan projectTools clone
		cloneRepository: self
]
