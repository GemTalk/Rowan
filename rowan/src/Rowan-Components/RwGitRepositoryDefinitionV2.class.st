Class {
	#name : 'RwGitRepositoryDefinitionV2',
	#superclass : 'RwDiskRepositoryDefinitionV2',
	#instVars : [
		'gitRoot',
		'remoteUrl'
	],
	#category : 'Rowan-Components'
}

{ #category : 'instance creation' }
RwGitRepositoryDefinitionV2 class >> newNamed: repositoryName forSpecification: aRwSpecification repositoryRoot: repoRoot [

	^ (super newNamed: repositoryName forSpecification: aRwSpecification repositoryRoot: repoRoot)
		committish: aRwSpecification repoSpec committish committishType: aRwSpecification repoSpec committishType;
		remoteUrl: aRwSpecification remoteUrl;
		yourself
]

{ #category : 'instance creation' }
RwGitRepositoryDefinitionV2 class >> newNamed: repositoryName repositoryRoot: repoRoot projectUrl: anUrlString remoteUrl: aRemoreUrlStrng committish: committish committishType:committishType [

	^ self new
		name: repositoryName;
		repositoryRoot: repoRoot;
		projectUrl: anUrlString;
		remoteUrl: aRemoreUrlStrng;
		committish: committish committishType:committishType;
		yourself
]

{ #category : 'private' }
RwGitRepositoryDefinitionV2 >> _createRemoteUrl [
	| aProjectUrl aRemoteUrl segments |
	self projectUrl isEmpty
		ifTrue: [ ^ nil ].
	aProjectUrl := RwUrl fromString: self projectUrl.
	aRemoteUrl := 'git@' , aProjectUrl authority , ':'.
	segments := aProjectUrl segments.
	aRemoteUrl := segments size = 1
		ifTrue: [ aRemoteUrl , (segments at: 1) , '.git ' ]
		ifFalse: [ aRemoteUrl , (segments at: 1) , '/' , (aProjectUrl segments at: 2) , '.git ' ].
	^ aRemoteUrl
]

{ #category : 'testing' }
RwGitRepositoryDefinitionV2 >> canCommit [

	^ true
]

{ #category : 'actions' }
RwGitRepositoryDefinitionV2 >> clone [

	"attach to existing cloned disk structure or clone project from remote repository"

	"who wins? resolve or clone"

	Rowan projectTools clone
		cloneRepository: self
]

{ #category : 'loading' }
RwGitRepositoryDefinitionV2 >> commitLog: logLimit [

	^ Rowan gitTools gitlogtool: 'HEAD' limit: logLimit gitRepoDirectory: self gitRoot pathString
]

{ #category : 'actions' }
RwGitRepositoryDefinitionV2 >> doCommit: message [

	| gitTool gitRootPath commitMessageFileName status |
	gitTool := Rowan gitTools.
	gitRootPath := self repositoryRoot pathString.
	commitMessageFileName := gitTool createTmpFileWith: message.
	gitTool gitaddIn: gitRootPath with: '-A .'.
	gitTool gitcommitIn: gitRootPath with: '--file=' , commitMessageFileName.
	status := gitTool gitlogIn: gitRootPath with: '-1'.
	Transcript
		cr;
		show: '==============';
		cr;
		show: status.
	^ status
]

{ #category : 'accessing' }
RwGitRepositoryDefinitionV2 >> gitRoot [
	"The root directory of the git repository that the project is located in. If the project is not git based
		or the git root is not explicitly assigned, git root is synonymous with repository root."

	^ self properties at: 'gitRoot' ifAbsent: [ self repositoryRoot ]
]

{ #category : 'accessing' }
RwGitRepositoryDefinitionV2 >> gitRoot: aGitRootReferenceOrString [

	aGitRootReferenceOrString ifNil: [ ^ self properties removeKey: 'gitRoot' ifAbsent: [] ].
	^ self properties at: 'gitRoot' put: aGitRootReferenceOrString asFileReference
]

{ #category : 'accessing' }
RwGitRepositoryDefinitionV2 >> remoteUrl [

	^ remoteUrl ifNil: [ remoteUrl := self _createRemoteUrl ]
]

{ #category : 'accessing' }
RwGitRepositoryDefinitionV2 >> remoteUrl: aRemoteUrlString [

	^ remoteUrl := aRemoteUrlString
]

{ #category : 'accessing' }
RwGitRepositoryDefinitionV2 >> repositoryCommitId [

	^ [  Rowan gitTools gitcommitShaIn: self gitRoot pathString ]
			on: Error
			do: [ :ignored | 
			"most likely no commits yet"
			'' ]
]

{ #category : 'actions' }
RwGitRepositoryDefinitionV2 >> resolve [

	"attach to existing git repository structure, clone, or create"

	"who wins? resolve or clone"

	Rowan projectTools clone
		cloneRepository: self
]
