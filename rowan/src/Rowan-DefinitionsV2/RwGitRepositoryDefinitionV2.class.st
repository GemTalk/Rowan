Class {
	#name : 'RwGitRepositoryDefinitionV2',
	#superclass : 'RwDiskRepositoryDefinitionV2',
	#instVars : [
		'remote',
		'remoteUrl',
		'committish',
		'gitUrl',
		'gitRoot',
		'relativeRepositoryRoot'
	],
	#category : 'Rowan-DefinitionsV2'
}

{ #category : 'instance creation' }
RwGitRepositoryDefinitionV2 class >> newNamed: repositoryName projectsHome: aFileReference repositoryUrl: aRepositoryUrlString relativeRepositoryRoot: aRelativeRepositoryRoot revision: revision [
	^ self new
		name: repositoryName;
		projectsHome: aFileReference;
		repositoryUrl: aRepositoryUrlString;
		relativeRepositoryRoot: aRelativeRepositoryRoot;
		committish: revision;
		yourself
]

{ #category : 'comparing' }
RwGitRepositoryDefinitionV2 >> = anObject [
	"remote remoteUrl committish"

	^ super = anObject
		and: [ 
			self remote = anObject remote
				and: [ 
					self remoteUrl = anObject remoteUrl
						and: [ 
							self relativeRepositoryRoot = anObject relativeRepositoryRoot
								and: [ 
									self committish = anObject committish
										and: [ self gitUrl = anObject gitUrl and: [ self gitRoot = anObject gitRoot ] ] ] ] ] ]
]

{ #category : 'actions' }
RwGitRepositoryDefinitionV2 >> branches [
	"return an array of branch names for the receiver"

	| str line branches remotesPrefix |
	self fetch.
	str := ReadStream
		on: (Rowan gitTools gitbranchIn: self gitRoot pathString with: '--all').
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
	Rowan gitTools gitcheckoutIn: self gitRoot with: aCommittish
]

{ #category : 'actions' }
RwGitRepositoryDefinitionV2 >> clone [

	"attach to existing cloned disk structure or clone project from remote repository"

	"who wins? resolve or clone"

	Rowan projectTools clone
		cloneRepository: self
]

{ #category : 'actions' }
RwGitRepositoryDefinitionV2 >> commitId [
	^ [ 
	| gitHome gitTools |
	gitTools := Rowan gitTools.
	gitHome := (gitTools gitrevparseShowTopLevelIn: self gitRoot pathString).
	gitTools gitcommitShaIn: gitHome ]
		on: Error
		do: [ :ignored | 
			"most likely no commits yet"
			'' ]
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
	Rowan gitTools gitcheckoutIn: self gitRoot with: '-b ' , branchName
]

{ #category : 'actions' }
RwGitRepositoryDefinitionV2 >> currentBranch [
	| result |
	result := Rowan gitTools gitBranchNameIn: self gitRoot pathString.
	(result findString: '(HEAD' startingAt: 1) > 0
		ifTrue: [ 
			"if detached head, return the sha of the commit"
			^ Rowan gitTools gitcommitShaIn: self gitRoot pathString ].
	^ result
]

{ #category : 'actions' }
RwGitRepositoryDefinitionV2 >> doCommit: message [
	| gitTool gitRootPath commitMessageFileName status |
	gitTool := Rowan gitTools.
	gitRootPath := self gitRoot pathString.
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

{ #category : 'actions' }
RwGitRepositoryDefinitionV2 >> fetch [
	Rowan gitTools gitfetchIn: self gitRoot with: self remote
]

{ #category : 'actions' }
RwGitRepositoryDefinitionV2 >> gitRepositoryRoot: repositoryRootPathString revision: aString [
	self
		repositoryUrl: 'file:' , repositoryRootPathString;
		repositoryRoot: repositoryRootPathString;
		committish: aString
]

{ #category : 'accessing' }
RwGitRepositoryDefinitionV2 >> gitRoot [
	^ gitRoot
		ifNil: [ 
			| root |
			root := self repositoryRoot.	"may initialize gitRoot"
			gitRoot ifNil: [ ^ root ].
			gitRoot ]
]

{ #category : 'accessing' }
RwGitRepositoryDefinitionV2 >> gitRoot: pathStringOrReference [
	gitRoot := pathStringOrReference asFileReference
]

{ #category : 'accessing' }
RwGitRepositoryDefinitionV2 >> gitUrl [
	"gitUrl specifies the repository root (!!) of an existing git repository -- if it is a file: url"

	^ gitUrl ifNil: [ self repositoryUrl ]
]

{ #category : 'accessing' }
RwGitRepositoryDefinitionV2 >> gitUrl: object [
	"gitUrl specifies the repository root (!!) of an existing git repository -- if it is a file: url"

	gitUrl := object
]

{ #category : 'comparing' }
RwGitRepositoryDefinitionV2 >> hash [
	| hashValue |
	hashValue := super hash.
	hashValue := hashValue bitXor: self remote hash.
	hashValue := hashValue bitXor: self remoteUrl hash.
	hashValue := hashValue bitXor: self committish hash.
	hashValue := hashValue bitXor: self gitUrl hash.
	hashValue := hashValue bitXor: self gitRoot hash.
	hashValue := hashValue bitXor: self relativeRepositoryRoot hash.
	^ hashValue
]

{ #category : 'actions' }
RwGitRepositoryDefinitionV2 >> pull: remoteName branch: branchName [
	Rowan gitTools
		gitpullIn: self gitRoot pathString
		remote: remoteName
		branch: branchName
]

{ #category : 'actions' }
RwGitRepositoryDefinitionV2 >> push: remoteName branch: branchName [
	Rowan gitTools
		gitpushIn: self gitRoot pathString
		remote: remoteName
		branch: branchName
]

{ #category : 'accessing' }
RwGitRepositoryDefinitionV2 >> relativeRepositoryRoot [
	^ relativeRepositoryRoot ifNil: [ ^ '' ]
]

{ #category : 'accessing' }
RwGitRepositoryDefinitionV2 >> relativeRepositoryRoot: object [
	relativeRepositoryRoot := object
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
	^ remoteUrl ifNil: [ remoteUrl := self repositoryUrl ]
]

{ #category : 'accessing' }
RwGitRepositoryDefinitionV2 >> remoteUrl: aRemoteUrlString [

	^ remoteUrl := aRemoteUrlString
]

{ #category : 'accessing' }
RwGitRepositoryDefinitionV2 >> repositoryRoot [
	"Root directory of the project. The componentsPath, specsPath, and projectsPath are specified relative to the repository root."

	^ repositoryRoot
		ifNil: [ 
			| repoRoot |
			repositoryUrl
				ifNotNil: [ :urlString | 
					| url |
					url := urlString asRwUrl.
					url scheme = 'file'
						ifTrue: [ 
							| urlPathString |
							urlPathString := url pathString.
							self relativeRepositoryRoot isEmpty not
								ifTrue: [ 
									gitRoot
										ifNil: [ 
											| repRootArray relRepRootArray index gitRootArray |
											"need to strip relativeRepositoryRoot off the back end of the url"
											relRepRootArray := self relativeRepositoryRoot asFileReference asPath
												segments.
											repRootArray := urlPathString asFileReference asPath segments.
											index := repRootArray indexOfSubCollection: relRepRootArray.
											gitRootArray := repRootArray copyFrom: 1 to: index - 1.
											self gitRoot: Path / (AbsolutePath withAll: gitRootArray) pathString ].
									self repositoryRoot: urlPathString ]
								ifFalse: [ self repositoryRoot: urlPathString ].
							^ repositoryRoot ] ].
			repoRoot := self projectsHome / self name.
			self relativeRepositoryRoot isEmpty not
				ifTrue: [ 
					gitRoot ifNil: [ self gitRoot: repoRoot ].
					repoRoot := repoRoot / self relativeRepositoryRoot ].
			self repositoryRoot: repoRoot.
			^ repositoryRoot ]
]

{ #category : 'accessing' }
RwGitRepositoryDefinitionV2 >> repositoryRoot: pathStringOrReference [
	| fileRef |
	super repositoryRoot: pathStringOrReference.
	gitRoot
		ifNil: [ 
			fileRef := pathStringOrReference asFileReference.
			fileRef exists
				ifTrue: [ self gitRoot: (Rowan gitTools gitrevparseShowTopLevelIn: fileRef pathString) ]
				ifFalse: [ 
					"assume we'll create and init a git repository at this location"
					self gitRoot: fileRef ] ]
]

{ #category : 'accessing' }
RwGitRepositoryDefinitionV2 >> repositoryUrl: aRepositoryUrlStrng [
	super repositoryUrl: aRepositoryUrlStrng.
	self remoteUrl: aRepositoryUrlStrng
]

{ #category : 'actions' }
RwGitRepositoryDefinitionV2 >> resolveRepository [
	"attach to existing repository structure or clone"

	"answer true if attaching to an existing repository"

	| url |
	url := self gitUrl.
	^ (url isEmpty or: [ url asRwUrl scheme = 'file' ])
		ifTrue: [ 
			| gitTool "Create a new git repository in repository root" |
			gitTool := Rowan projectTools git.
			self gitRoot ensureCreateDirectory.
			(gitTool gitPresentIn: self gitRoot pathString)
				ifTrue: [ true ]
				ifFalse: [ 
					"create a git repository"
					gitTool gitinitIn: self gitRoot pathString with: ''.
					false ] ]
		ifFalse: [ 
			"clone from remote"
			self clone.
			true ]
]

{ #category : 'accessing' }
RwGitRepositoryDefinitionV2 >> revision [
	^ [ Rowan gitTools gitcommitShaIn: self gitRoot pathString ]
		on: Error
		do: [ :ignored | 
			"most likely no commits yet"
			'' ]
]

{ #category : 'actions' }
RwGitRepositoryDefinitionV2 >> updateLoadSpecWithRepositoryRoot: aLoadSpec [
	"preserve the current repositoryRoot in the loadSpec"

	aLoadSpec gitUrl: 'file:', self repositoryRoot pathString
]

{ #category : 'testing' }
RwGitRepositoryDefinitionV2 >> useGit [
	^ true
]
