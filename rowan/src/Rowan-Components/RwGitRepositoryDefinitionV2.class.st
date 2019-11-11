Class {
	#name : 'RwGitRepositoryDefinitionV2',
	#superclass : 'RwDiskRepositoryDefinitionV2',
	#instVars : [
		'remoteUrl'
	],
	#category : 'Rowan-Components'
}

{ #category : 'instance creation' }
RwGitRepositoryDefinitionV2 class >> newNamed: repositoryName repositoryRoot: repoRoot projectUrl: aProjectUrlStrng committish: committish [
	^ self new
		name: repositoryName;
		repositoryRoot: repoRoot;
		projectUrl: aProjectUrlStrng;
		committish: committish;
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
RwGitRepositoryDefinitionV2 >> projectUrl: aProjectUrlStrng [

	| aProjectUrl aRemoteUrl segments |
	aProjectUrlStrng
		ifTrue: [ ^ self ].
	aProjectUrl := RwUrl fromString: aProjectUrlStrng.
	aRemoteUrl := 'git@' , aProjectUrl authority , ':'.
	segments := aProjectUrl segments.
	aRemoteUrl := segments size = 1
		ifTrue: [ aRemoteUrl , (segments at: 1) , '.git ' ]
		ifFalse: [ aRemoteUrl , (segments at: 1) , '/' , (aProjectUrl segments at: 2) , '.git ' ].
	^ self remoteUrl: aRemoteUrl
]

{ #category : 'accessing' }
RwGitRepositoryDefinitionV2 >> remoteUrl [

	^ remoteUrl
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
	"attach to existing repository structure or create"

	self repositoryRoot exists
		ifFalse: [ self clone ]
]
