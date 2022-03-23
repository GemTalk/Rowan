Class {
	#name : 'RwGitTool',
	#superclass : 'RwAbstractTool',
	#category : 'Rowan-Tools-Core'
}

{ #category : 'private' }
RwGitTool >> _mktempCommand [
	^ (System gemVersionReport at: 'osName') = 'Darwin'
		ifTrue: [ '/usr/bin/mktemp -t commitMessage' ]
		ifFalse: [ '/bin/mktemp --tmpdir commitMessage.XXXX' ]
]

{ #category : 'private' }
RwGitTool >> _shouldLog [
	^ Rowan projectTools trace isTracing
]

{ #category : 'smalltalk api' }
RwGitTool >> gitaddIn: gitRepoPath with: args [

	^ self performGitCommand: 'add' in: gitRepoPath with: args
]

{ #category : 'smalltalk api' }
RwGitTool >> gitbranchIn: gitRepoPath with: args [

	^ self performGitCommand: 'branch' in: gitRepoPath with: args
]

{ #category : 'smalltalk api' }
RwGitTool >> gitBranchNameIn: gitRepoPath [
	"return current branch for git repository located at gitPath"

	| command result |
	command := 'set -e; cd ' , gitRepoPath , ';git branch | sed -n ''/\* /s///p'''.
	result := self performOnServer: command logging: self _shouldLog.
	^ result trimWhiteSpace
]

{ #category : 'smalltalk api' }
RwGitTool >> gitcheckoutIn: gitRootReference with: args [
	^ self performGitCommand: 'checkout' in: gitRootReference pathString with: args
]

{ #category : 'smalltalk api' }
RwGitTool >> gitcloneIn: gitRootPath with: args [

	^ self gitcloneIn: gitRootPath with: args logging: self _shouldLog
]

{ #category : 'smalltalk api' }
RwGitTool >> gitcloneIn: gitRootPath with: gitArgs logging: logging [

	| commandBase command gitCommand |
	gitCommand := 'clone'.
	commandBase := 'set -e; cd ' , gitRootPath , '; git ' , gitCommand , ' '.
	command := commandBase , gitArgs.
	^ self performOnServer: command logging: logging
]

{ #category : 'smalltalk api' }
RwGitTool >> gitcommitIn: gitRepoPath with: args [

	^ self performGitCommand: 'commit' in: gitRepoPath with: args
]

{ #category : 'smalltalk api' }
RwGitTool >> gitcommitShaIn: gitRepoPath [

	| result |
	gitRepoPath ifNil: [ ^ '' ].
	result := self gitlogIn: gitRepoPath with: ' -1 --pretty=format:%h '.
	(result beginsWith: 'fatal:')
		ifTrue: [ ^ nil ].
	^ result trimWhiteSpace
]

{ #category : 'smalltalk api' }
RwGitTool >> gitfetchIn: gitRootReference with: args [
	^ self performGitCommand: 'fetch' in: gitRootReference pathString with: args
]

{ #category : 'smalltalk api' }
RwGitTool >> gitinitIn: dirPath with: args [

	| command |
	command := 'set -e; cd ' , dirPath , '; git init ' , args.
	^ self performOnServer: command logging: self _shouldLog
]

{ #category : 'smalltalk api' }
RwGitTool >> gitlogFull: commitish limit: limit gitRepoDirectory: gitRepoPath [

	| commitLog |
	commitLog := self
		gitlogIn: gitRepoPath
		with: ' -' , limit printString , ' ' , commitish.
	^ commitLog
]

{ #category : 'smalltalk api' }
RwGitTool >> gitlogIn: gitRepoPath with: args [

	^ self performGitCommand: 'log' in: gitRepoPath with: args
]

{ #category : 'smalltalk api' }
RwGitTool >> gitlogtool: commitish limit: limit gitRepoDirectory: gitRepoPath [

	^ self
		gitlogIn: gitRepoPath
		with:
			'--date=relative --format="%h %cd %s" -' , limit printString , ' ' , commitish
]

{ #category : 'smalltalk api' }
RwGitTool >> gitPresentIn: gitRepoPath [

	| gitHome command cdResponse |
	[ 
	gitHome := self gitrevparseShowTopLevelIn: gitRepoPath.
	command := 'set -e; cd ' , gitRepoPath , '; pwd'.
	cdResponse := self performOnServer: command logging: self _shouldLog ]
		on: Error
		do: [ :ex | ^ false ].
	^ (self readlink: gitHome) = (self readlink: cdResponse)
]

{ #category : 'smalltalk api' }
RwGitTool >> gitpullIn: gitRepoPath [

	^ self gitpullIn: gitRepoPath with: ''
]

{ #category : 'smalltalk api' }
RwGitTool >> gitpullIn: gitRepoPath remote: remoteName branch: branchName [

	^ self gitpullIn: gitRepoPath with: remoteName , ' ' , branchName
]

{ #category : 'smalltalk api' }
RwGitTool >> gitpullIn: gitRepoPath with: args [

	^ self performGitCommand: 'pull' in: gitRepoPath with: args
]

{ #category : 'smalltalk api' }
RwGitTool >> gitpushIn: gitRepoPath remote: remoteName branch: branchName [

	^ self gitpushIn: gitRepoPath with: remoteName , ' ' , branchName
]

{ #category : 'smalltalk api' }
RwGitTool >> gitpushIn: gitRepoPath with: args [

	^ self performGitCommand: 'push' in: gitRepoPath with: args
]

{ #category : 'smalltalk api' }
RwGitTool >> gitrevparseShowTopLevelIn: dirPath [

	| command |
	command := 'set -e; cd ' , dirPath , '; git rev-parse --show-toplevel'.
	^ self performOnServer: command logging: self _shouldLog
]

{ #category : 'smalltalk api' }
RwGitTool >> gitstatusIn: gitRepoPath with: args [

	^ self performGitCommand: 'status' in: gitRepoPath with: args
]

{ #category : 'smalltalk api' }
RwGitTool >> isGitHome: homeReferenceOrPath [
	"Answer true if the given homeReferenceOrPath is the home directory for a git repository"

	| homeFileReference expandedHomePath expandedGitRootPath |
	homeFileReference := homeReferenceOrPath asFileReference.
	(self gitPresentIn: homeFileReference pathString)
		ifFalse: [ 
			"early check needed to protect gitrevparseShowTopLevelIn: call"
			^ false ].
	expandedHomePath := self readlink: homeFileReference pathString.
	expandedGitRootPath := self
		readlink:
			(self gitrevparseShowTopLevelIn: homeFileReference pathString) trimBoth.
	^ (self gitPresentIn: expandedGitRootPath)
		and: [ expandedHomePath = expandedGitRootPath ]
]

{ #category : 'private' }
RwGitTool >> performGitCommand: gitCommand in: gitRepoPath with: gitArgs [

	^ self
		performGitCommand: gitCommand
		in: gitRepoPath
		worktree: gitRepoPath
		with: gitArgs
		logging: self _shouldLog
]

{ #category : 'private' }
RwGitTool >> performGitCommand: gitCommand in: gitRepoPath worktree: workTreePath with: gitArgs logging: logging [

	"unconditional cd to work-tree because of git pull problem with git prior to 1.7.7.2"

	| commandBase command |
	commandBase := 'set -e; cd ' , workTreePath , '; git --git-dir ' , gitRepoPath
		, '/.git --work-tree ' , workTreePath , ' ' , gitCommand , ' '.
	command := commandBase , gitArgs.
	^ self performOnServer: command logging: logging
]

{ #category : 'private' }
RwGitTool >> performOnServer: commandLine [
	^ self performOnServer: commandLine logging: self _shouldLog
]

{ #category : 'private' }
RwGitTool >> performOnServer: commandLine logging: logging [
  | result |
  result := self
    performOnServer: commandLine
    status: [ :performOnServerStatusArray | 
      "Array of 5 elements: 
       raw status Integer, 
       child process status Integer (after WEXITSTATUS macro applied), 
       result String (or nil if operation failed) ,
       error string from script file write, fork, or result file read ,
       errno value, a SmallInteger from file write, fork, or file read"
      (performOnServerStatusArray at: 1) ~~ 0
        ifTrue: [ 
          | message |
          message := 'performOnServer: ' , commandLine printString , ' stdout: '
            , (performOnServerStatusArray at: 3) printString
            , ' failed with status: '
            , (performOnServerStatusArray at: 1) printString , ' errno: '
            , (performOnServerStatusArray at: 5) printString , ' errStr: '
            , (performOnServerStatusArray at: 4) asString.
          self error: message ].
      performOnServerStatusArray at: 3 ].
  logging
    ifTrue: [ 
      Transcript
        cr;
        show: commandLine printString;
        cr;
        show: result ].
  ^ result

]
