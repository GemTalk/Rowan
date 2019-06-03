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
	self projectUrl isEmpty ifTrue:  [ ^ nil ].
	projectUrl := RwUrl fromString: self projectUrl.
	remoteUrl := 'git@' , projectUrl authority , ':'.
	segments := projectUrl segments.
	remoteUrl := segments size = 1
		ifTrue: [ remoteUrl , (segments at: 1) , '.git ' ]
		ifFalse: [ remoteUrl , (segments at: 1) , '/' , (projectUrl segments at: 2) , '.git ' ].
	^ remoteUrl
]

{ #category : 'testing' }
RwGitRepositoryDefinition >> canCommit [

	^ true
]

{ #category : 'actions' }
RwGitRepositoryDefinition >> clone [

	"attach to existing cloned disk structure or clone project from remote repository"

	"who wins? resolve or clone"

	Rowan projectTools clone
		cloneRepository: self
]

{ #category : 'accessing' }
RwGitRepositoryDefinition >> commitId [

	^ [  Rowan gitTools gitcommitShaIn: self gitRoot pathString ]
		on: Error
		do: [ :ignored | 
			"most likely no commits yet"
			'' ].
]

{ #category : 'loading' }
RwGitRepositoryDefinition >> commitLog: logLimit [

	^ Rowan gitTools gitlogtool: 'HEAD' limit: logLimit gitRepoDirectory: self gitRoot pathString
]

{ #category : 'accessing' }
RwGitRepositoryDefinition >> gitRoot [
	"The root directory of the git repository that the project is located in. If the project is not git based
		or the git root is not explicitly assigned, git root is synonymous with repository root."

	^ self properties at: 'gitRoot' ifAbsent: [ self repositoryRoot ]
]

{ #category : 'accessing' }
RwGitRepositoryDefinition >> gitRoot: aGitRootReferenceOrString [

	aGitRootReferenceOrString ifNil: [ ^ self properties removeKey: 'gitRoot' ifAbsent: [] ].
	^ self properties at: 'gitRoot' put: aGitRootReferenceOrString asFileReference
]

{ #category : 'accessing' }
RwGitRepositoryDefinition >> remoteUrl [

	^ self properties at: 'remoteUrl' ifAbsentPut: [ self _createRemoteUrl ]
]

{ #category : 'accessing' }
RwGitRepositoryDefinition >> remoteUrl: aRemoteUrlString [

	^ self properties at: 'remoteUrl' put: aRemoteUrlString
]

{ #category : 'accessing' }
RwGitRepositoryDefinition >> repositoryCommitId [

	^ [  Rowan gitTools gitcommitShaIn: self gitRoot pathString ]
			on: Error
			do: [ :ignored | 
			"most likely no commits yet"
			'' ]
]

{ #category : 'actions' }
RwGitRepositoryDefinition >> resolve [

	"attach to existing git repository structure, clone, or create"

	"who wins? resolve or clone"

	Rowan projectTools clone
		cloneRepository: self
]
