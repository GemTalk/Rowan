Class {
	#name : 'RwGitRepositoryDefinitionV2',
	#superclass : 'RwDiskRepositoryDefinitionV2',
	#instVars : [
		'remoteUrl',
		'committish'
	],
	#category : 'Rowan-Components'
}

{ #category : 'instance creation' }
RwGitRepositoryDefinitionV2 class >> newNamed: repositoryName projectsHome: aFileReference repositoryUrl: aRepositoryUrlString revision: revision [
	^ self new
		name: repositoryName;
		projectsHome: aFileReference;
		repositoryUrl: aRepositoryUrlString;
		committish: revision;
		yourself
]

{ #category : 'private' }
RwGitRepositoryDefinitionV2 >> _createRemoteUrl [
	| aRepositoryUrl aRemoteUrl segments |
	self repositoryUrl isEmpty
		ifTrue: [ ^ nil ].
	aRepositoryUrl := RwUrl fromString: self  repositoryUrl.
	aRemoteUrl := 'git@' , aRepositoryUrl authority , ':'.
	segments := aRepositoryUrl segments.
	aRemoteUrl := segments size = 1
		ifTrue: [ aRemoteUrl , (segments at: 1) , '.git ' ]
		ifFalse: [ aRemoteUrl , (segments at: 1) , '/' , (aRepositoryUrl segments at: 2) , '.git ' ].
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

{ #category : 'accessing' }
RwGitRepositoryDefinitionV2 >> committish [
	^ committish ifNil: [ '' ]
]

{ #category : 'accessing' }
RwGitRepositoryDefinitionV2 >> committish: aString [

	committish := aString
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
RwGitRepositoryDefinitionV2 >> remoteUrl [
	^ remoteUrl ifNil: [ remoteUrl := self _createRemoteUrl ]
]

{ #category : 'accessing' }
RwGitRepositoryDefinitionV2 >> remoteUrl: aRemoteUrlString [

	^ remoteUrl := aRemoteUrlString
]

{ #category : 'accessing' }
RwGitRepositoryDefinitionV2 >> repositoryUrl: aRepositoryUrlStrng [
	super repositoryUrl: aRepositoryUrlStrng.
	self remoteUrl: self _createRemoteUrl
]

{ #category : 'actions' }
RwGitRepositoryDefinitionV2 >> resolve [
	"attach to existing repository structure or create"

	self repositoryUrl asRwUrl scheme = 'file'
		ifTrue: [ 
			| gitTool "Creatr a new git repository in repository root" |
			gitTool := Rowan projectTools git.
			self repositoryRoot ensureCreateDirectory.
			(gitTool gitPresentIn: self repositoryRoot pathString)
				ifFalse: [ 
					"create a git repository"
					gitTool gitinitIn: self repositoryRoot pathString with: '' ] ]
		ifFalse: [ 
			"clone from remote"
			self clone ]
]

{ #category : 'accessing' }
RwGitRepositoryDefinitionV2 >> revision [
	^ [ Rowan gitTools gitcommitShaIn: self repositoryRoot pathString ]
		on: Error
		do: [ :ignored | 
			"most likely no commits yet"
			'' ]
]
