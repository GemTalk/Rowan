Class {
	#name : 'RwPrjGitTool',
	#superclass : 'RwProjectTool',
	#instVars : [
		'tdGitTool'
	],
	#category : 'Rowan-Tools'
}

{ #category : 'smalltalk api' }
RwPrjGitTool >> createTmpFileWith: fileContents [

	| file filename |
	filename := '/tmp/commitMessage'.
	[ 
	| count |
	file := GsFile openWriteOnServer: filename.
	(count := file nextPutAll: fileContents withGemstoneLineEndings)
		ifNil: [ self error: 'failed write' ] ]
		ensure: [ file close ].
	^ filename
]

{ #category : 'smalltalk api' }
RwPrjGitTool >> gitaddIn: gitRepoPath with: args [

	^ self performGitCommand: 'add' in: gitRepoPath with: args
]

{ #category : 'smalltalk api' }
RwPrjGitTool >> gitbranchIn: gitRepoPath with: args [

	^ self performGitCommand: 'branch' in: gitRepoPath with: args
]

{ #category : 'smalltalk api' }
RwPrjGitTool >> gitBranchNameIn: gitRepoPath [

	"return current branch for git repository located at gitPath"

	| command result |
	command := 'set -e; cd ' , gitRepoPath , '; git rev-parse --abbrev-ref HEAD'.
	result := System performOnServer: command.
	^ result trimWhiteSpace
]

{ #category : 'smalltalk api' }
RwPrjGitTool >> gitcheckoutIn: gitRepoPath with: args [

	^ self performGitCommand: 'checkout' in: gitRepoPath with: args
]

{ #category : 'smalltalk api' }
RwPrjGitTool >> gitcloneIn: gitRootPath with: args [

	^ self gitcloneIn: gitRootPath with: args logging: true
]

{ #category : 'smalltalk api' }
RwPrjGitTool >> gitcloneIn: gitRootPath with: gitArgs logging: logging [

	| commandBase command gitCommand |
	gitCommand := 'clone'.
	commandBase := 'set -e; cd ' , gitRootPath , '; git ' , gitCommand , ' '.
	command := commandBase , gitArgs.
	^ self performOnServer: command logging: logging
]

{ #category : 'smalltalk api' }
RwPrjGitTool >> gitcommitIn: gitRepoPath with: args [

	^ self performGitCommand: 'commit' in: gitRepoPath with: args
]

{ #category : 'smalltalk api' }
RwPrjGitTool >> gitcommitShaIn: gitRepoPath [

	| result |
	result := self gitlogIn: gitRepoPath with: ' -1 --pretty=format:%h '.
	(result beginsWith: 'fatal:')
		ifTrue: [ ^ nil ].
	^ result trimWhiteSpace
]

{ #category : 'smalltalk api' }
RwPrjGitTool >> gitinitIn: dirPath with: args [

	| command |
	command := 'set -e; cd ' , dirPath , '; git init ' , args.
	^ self performOnServer: command logging: true
]

{ #category : 'smalltalk api' }
RwPrjGitTool >> gitlogIn: gitRepoPath with: args [

	^ self performGitCommand: 'log' in: gitRepoPath with: args
]

{ #category : 'smalltalk api' }
RwPrjGitTool >> gitlogtool: commitish limit: limit gitRepoDirectory: gitRepoPath [

	^ self
		gitlogIn: gitRepoPath
		with:
			'--date=relative --format="%h %cd %s" -' , limit printString , ' ' , commitish
]

{ #category : 'smalltalk api' }
RwPrjGitTool >> gitPresentIn: gitRepoPath [

	| gitHome command cdResponse |
	[ 
	gitHome := self gitrevparseShowTopLevelIn: gitRepoPath.
	command := 'set -e; cd ' , gitRepoPath , '; pwd'.
	cdResponse := self performOnServer: command logging: true ]
		on: Error
		do: [ :ex | ^ false ].
	^ gitHome = cdResponse
]

{ #category : 'smalltalk api' }
RwPrjGitTool >> gitpullIn: gitRepoPath [

	^ self gitpullIn: gitRepoPath with: ''
]

{ #category : 'smalltalk api' }
RwPrjGitTool >> gitpullIn: gitRepoPath remote: remoteName branch: branchName [

	^ self gitpullIn: gitRepoPath with: remoteName , ' ' , branchName
]

{ #category : 'smalltalk api' }
RwPrjGitTool >> gitpullIn: gitRepoPath with: args [

	^ self performGitCommand: 'pull' in: gitRepoPath with: args
]

{ #category : 'smalltalk api' }
RwPrjGitTool >> gitpushIn: gitRepoPath remote: remoteName branch: branchName [

	^ self gitpushIn: gitRepoPath with: remoteName , ' ' , branchName
]

{ #category : 'smalltalk api' }
RwPrjGitTool >> gitpushIn: gitRepoPath with: args [

	^ self performGitCommand: 'push' in: gitRepoPath with: args
]

{ #category : 'smalltalk api' }
RwPrjGitTool >> gitrevparseShowTopLevelIn: dirPath [

	| command |
	command := 'set -e; cd ' , dirPath , '; git rev-parse --show-toplevel'.
	^ self performOnServer: command logging: true
]

{ #category : 'private' }
RwPrjGitTool >> performGitCommand: gitCommand in: gitRepoPath with: gitArgs [

	^ self
		performGitCommand: gitCommand
		in: gitRepoPath
		worktree: gitRepoPath
		with: gitArgs
		logging: true
]

{ #category : 'private' }
RwPrjGitTool >> performGitCommand: gitCommand in: gitRepoPath worktree: workTreePath with: gitArgs logging: logging [

	"unconditional cd to work-tree because of git pull problem with git prior to 1.7.7.2"

	| commandBase command |
	commandBase := 'set -e; cd ' , workTreePath , '; git --git-dir ' , gitRepoPath
		, '/.git --work-tree ' , workTreePath , ' ' , gitCommand , ' '.
	command := commandBase , gitArgs.
	^ self performOnServer: command logging: logging
]

{ #category : 'private' }
RwPrjGitTool >> performOnServer: commandLine logging: logging [
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

{ #category : 'private' }
RwPrjGitTool >> performOnServer: commandLine status: statusBlock [
  | performOnServerStatusArray |
  performOnServerStatusArray := System _performOnServer: commandLine.
  ^ statusBlock value: performOnServerStatusArray
]
