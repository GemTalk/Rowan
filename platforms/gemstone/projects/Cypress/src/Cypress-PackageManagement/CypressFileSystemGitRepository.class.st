"
All Cypress classes are private to GemStone and are likely to be removed in a future release.
"
Class {
	#name : 'CypressFileSystemGitRepository',
	#superclass : 'CypressFileSystemRepository',
	#instVars : [
		'remoteUrl'
	],
	#category : 'Cypress-PackageManagement'
}

{ #category : 'accessing' }
CypressFileSystemGitRepository class >> gitRepositoryDir [
  | path |
  path := (SessionTemps current at: #'Cypress_FileSystem_Git_Repository_Directory' otherwise: nil)
    ifNil: [
      path := (System gemEnvironmentVariable: 'GEMSTONE_GITDIR')
        ifNil: [ 
          CypressFileUtilities current workingDirectory
          , CypressFileUtilities current pathNameDelimiter , 'cypress-git-repos' ].
      SessionTemps current at: #'Cypress_FileSystem_Git_Repository_Directory' put: path ].
  ^path

]

{ #category : 'accessing' }
CypressFileSystemGitRepository class >> gitRepositoryDir: directoryPath [
  ^ SessionTemps current at: #'Cypress_FileSystem_Git_Repository_Directory' put: directoryPath

]

{ #category : 'git commands' }
CypressFileSystemGitRepository class >> performOnServer: commandLine [
  | result |
  result := self
    performOnServer: commandLine
    status: [ :performOnServerStatusArray | "Array of 5 elements: 
       raw status Integer, 
       child process status Integer (after WEXITSTATUS macro applied), 
       result String (or nil if operation failed) ,
       error string from script file write, fork, or result file read ,
       errno value, a SmallInteger from file write, fork, or file read"
      (performOnServerStatusArray at: 1) ~~ 0
        ifTrue: [ | message |
          message := 'performOnServer: ' , commandLine printString , ' stdout: '
            , (performOnServerStatusArray at: 3) printString
            , ' failed with status: '
            , (performOnServerStatusArray at: 1) printString , ' errno: '
            , (performOnServerStatusArray at: 5) printString , ' errStr: '
            , (performOnServerStatusArray at: 4) asString.
          self error: message ].
      performOnServerStatusArray at: 3 ].
  Transcript
    cr;
    show: commandLine printString;
    cr;
    show: result.
  ^ result

]

{ #category : 'git commands' }
CypressFileSystemGitRepository class >> performOnServer: commandLine status: statusBlock [
  | performOnServerStatusArray |
  performOnServerStatusArray := System _performOnServer: commandLine.
  ^ statusBlock value: performOnServerStatusArray

]

{ #category : 'git commands' }
CypressFileSystemGitRepository class >> runGitCommand: argsArray in: gitRootPath [

	| stream |
	stream := WriteStreamPortable on: (String new: 100).
	stream nextPutAll: 'cd ' , gitRootPath , '; git '.
	argsArray do: 
			[:arg |
			stream
				space;
				nextPutAll: arg].
	^self performOnServer: stream contents

]

{ #category : 'accessing' }
CypressFileSystemGitRepository >> currentBranch [
  | result |
  result := self
    gitCommand: #('rev-parse' '--abbrev-ref' 'HEAD')
    in: self directoryPath.
  ^ result trimWhiteSpace

]

{ #category : 'git querying' }
CypressFileSystemGitRepository >> gitCloneRepositoryAndCheckoutIn: aDirectoryName workingDirectory: workingDirectory [
  "Do a clone on the remote repository and do a checkout on it to get the right branch. Complement the directory as well with the working directory."

  | gitDir branch |
  gitDir := workingDirectory , self fileUtils pathNameDelimiter
    , (aDirectoryName subStrings: '/') last.
  branch := self projectBranchOrTag.
  (self fileUtils directoryExists: gitDir)
    ifTrue: [ | targetDirBranch |
      self directoryPath: (self repositoryPath isEmpty
            ifTrue: [ gitDir ]
            ifFalse: [ gitDir , self fileUtils pathNameDelimiter , self repositoryPath ]).
      targetDirBranch := self currentBranch.
      targetDirBranch = branch
        ifFalse: [ self error: self class name asString
                ,
                  ' target directory already exists and is on another branch, cancelling clone and repository instanciation : '
                , gitDir ] ]
    ifFalse: [ self gitCommand: #('clone') , {'-b'.
              branch} , {remoteUrl.
              gitDir} in: workingDirectory.
      self directoryPath: (self repositoryPath isEmpty
            ifTrue: [ gitDir ]
            ifFalse: [ gitDir , self fileUtils pathNameDelimiter , self repositoryPath ]) ]

]

{ #category : 'git querying' }
CypressFileSystemGitRepository >> gitCommand: aCommandString in: aDirectory [
  ^ self class runGitCommand: aCommandString in: aDirectory

]

{ #category : 'initializing - private' }
CypressFileSystemGitRepository >> initializeRepositoryDirectory [
  self remoteUrl: self url httpsAccessString.
  self
    gitCloneRepositoryAndCheckoutIn: self projectPath
    workingDirectory: self class gitRepositoryDir.
  (self isGitRepository: self directoryPath)
    ifFalse: [ self error: 'This url is not a git repository' , self url printString ]

]

{ #category : 'git querying' }
CypressFileSystemGitRepository >> isGitRepository: aDirectory [
  "Check that we have a git repository"

  | gitPath |
  gitPath := self gitCommand: #('rev-parse' '--show-toplevel') in: aDirectory.
  (gitPath indexOfSubCollection: 'fatal:' startingAt: 1 ifAbsent: [ 0 ]) = 1
    ifTrue: [ ^ false ].
  ^ true

]

{ #category : 'accessing' }
CypressFileSystemGitRepository >> projectBranchOrTag [
  "right now only expect to work with branches"

  ^ self url projectBranchOrTag

]

{ #category : 'accessing' }
CypressFileSystemGitRepository >> projectPath [
  ^ self url projectPath

]

{ #category : 'accessing' }
CypressFileSystemGitRepository >> remoteUrl [

   ^remoteUrl

]

{ #category : 'accessing' }
CypressFileSystemGitRepository >> remoteUrl: anObject [

   remoteUrl := anObject

]

{ #category : 'accessing' }
CypressFileSystemGitRepository >> repositoryPath [
  ^ self url repositoryPath

]

{ #category : 'validating - private' }
CypressFileSystemGitRepository >> validateUrl [
  "At this level, there is nothing to check.
	 But different URLs denote different kinds of repositories, and
	 each kind of repository may have specific checks."

  (self url projectPath isEmpty or: [ self url projectBranchOrTag isEmpty ])
    ifTrue: [ self error: self printString , ' should not be used with non-git URLs.' ]

]
