Class {
	#name : 'RwGitRepositoryDefinitionV2',
	#superclass : 'RwDiskRepositoryDefinitionV2',
	#instVars : [
		'remote',
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

{ #category : 'actions' }
RwGitRepositoryDefinitionV2 >> branches [
	"return an array of branch names for the receiver"

	| str line branches remotesPrefix |
	self fetch.
	str := ReadStream
		on: (Rowan gitTools gitbranchIn: self repositoryRoot pathString with: '--all').
	branches := {}.
	remotesPrefix := 'remotes/origin/'.
	[ str atEnd ]
		whileFalse: [ 
			line := str nextLine trimBlanks.
			(line at: 1) = $*
				ifTrue: [ 
					"ignore current branch"
					 ]
				ifFalse: [ 
					| index |
					index := line findString: remotesPrefix startingAt: 1.
					index > 0
						ifTrue: [ line := (line copyFrom: index + remotesPrefix size to: line size) trimBlanks ].
					(line findString: 'HEAD' startingAt: 1) = 0
						ifTrue: [ branches add: line ] ] ].
	^ branches
]

{ #category : 'testing' }
RwGitRepositoryDefinitionV2 >> canCommit [

	^ true
]

{ #category : 'actions' }
RwGitRepositoryDefinitionV2 >> checkAndUpdateRepositoryRevision: aRwProjectLoadSpecificationV2 [
	| revision |
	revision := aRwProjectLoadSpecificationV2 revision.
	self currentBranch = revision
		ifTrue: [ ^ self ].
	(aRwProjectLoadSpecificationV2 isStrict
		or: [ 
			RwAllowChangeRepositoryRevisionOnResolveNotification
				signal: aRwProjectLoadSpecificationV2 ])
		ifTrue: [ 
			self fetch.
			self checkout: revision ]
]

{ #category : 'actions' }
RwGitRepositoryDefinitionV2 >> checkout: aCommittish [
	Rowan gitTools gitcheckoutIn: self repositoryRoot with: aCommittish
]

{ #category : 'actions' }
RwGitRepositoryDefinitionV2 >> clone [

	"attach to existing cloned disk structure or clone project from remote repository"

	"who wins? resolve or clone"

	Rowan projectTools clone
		cloneRepository: self
]

{ #category : 'actions' }
RwGitRepositoryDefinitionV2 >> commit: message [
	| gitTool gitRootPath commitMessageFileName status |
	gitTool := Rowan gitTools.
	gitRootPath := self repositoryRoot pathString.
	(gitTool gitstatusIn: gitRootPath with: '--short') isEmpty
		ifTrue: [ 
			| msg |
			msg := 'Nothing to commit'.
			Transcript
				cr;
				show: '==============';
				cr;
				show: msg.
			self inform: msg.
			^ msg ].
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
RwGitRepositoryDefinitionV2 >> createBranch: branchName [
	Rowan gitTools gitcheckoutIn: self repositoryRoot with: '-b ' , branchName
]

{ #category : 'actions' }
RwGitRepositoryDefinitionV2 >> currentBranch [
	| result |
	result := Rowan gitTools gitBranchNameIn: self repositoryRoot pathString.
	(result findString: '(HEAD' startingAt: 1) > 0
		ifTrue: [ 
			"if detached head, return the sha of the commit"
			^ Rowan gitTools gitcommitShaIn: self repositoryRoot pathString ].
	^ result
]

{ #category : 'actions' }
RwGitRepositoryDefinitionV2 >> fetch [
	Rowan gitTools gitfetchIn: self repositoryRoot with: self remote
]

{ #category : 'actions' }
RwGitRepositoryDefinitionV2 >> pull: remoteName branch: branchName [
	Rowan gitTools
		gitpullIn: self repositoryRoot pathString
		remote: remoteName
		branch: branchName
]

{ #category : 'actions' }
RwGitRepositoryDefinitionV2 >> push: remoteName branch: branchName [
	Rowan gitTools
		gitpushIn: self repositoryRoot pathString
		remote: remoteName
		branch: branchName
]

{ #category : 'accessing' }
RwGitRepositoryDefinitionV2 >> remote [
	^ remote ifNil: [ 'origin' ]
]

{ #category : 'accessing' }
RwGitRepositoryDefinitionV2 >> remote: remoteName [
	remote := remoteName
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

	"answer true if attaching to an existing repository"

	^ self repositoryUrl asRwUrl scheme = 'file'
		ifTrue: [ 
			| gitTool "Creatr a new git repository in repository root" |
			gitTool := Rowan projectTools git.
			self repositoryRoot ensureCreateDirectory.
			(gitTool gitPresentIn: self repositoryRoot pathString)
				ifTrue: [ true ]
				ifFalse: [ 
					"create a git repository"
					gitTool gitinitIn: self repositoryRoot pathString with: ''.
					false ] ]
		ifFalse: [ 
			"clone from remote"
			self clone.
			true ]
]

{ #category : 'accessing' }
RwGitRepositoryDefinitionV2 >> revision [
	^ [ Rowan gitTools gitcommitShaIn: self repositoryRoot pathString ]
		on: Error
		do: [ :ignored | 
			"most likely no commits yet"
			'' ]
]
