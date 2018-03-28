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
  ^ self tdGitTool createTmpFileWith: fileContents
]

{ #category : 'smalltalk api' }
RwPrjGitTool >> gitaddIn: gitRepoDir with: args [
  ^ self tdGitTool gitaddIn: gitRepoDir with: args
]

{ #category : 'smalltalk api' }
RwPrjGitTool >> gitBranchNameIn: gitRepoDir [
  "return current branch for git repository located at gitPath"

  | command result |
  command := 'set -e; cd ' , gitRepoDir fullName
    , '; git rev-parse --abbrev-ref HEAD'.
  result := System performOnServer: command.
  ^ result trimWhiteSpace
]

{ #category : 'smalltalk api' }
RwPrjGitTool >> gitcheckoutIn: gitRepoDir with: args [
  ^ self tdGitTool gitcheckoutIn: gitRepoDir with: args
]

{ #category : 'smalltalk api' }
RwPrjGitTool >> gitcloneIn: gitRootDirectory with: args [
  ^ self tdGitTool gitcloneIn: gitRootDirectory with: args
]

{ #category : 'smalltalk api' }
RwPrjGitTool >> gitcommitIn: gitRepoDir with: args [
  ^ self tdGitTool gitcommitIn: gitRepoDir with: args
]

{ #category : 'smalltalk api' }
RwPrjGitTool >> gitcommitShaIn: gitRepoDir [
  | result |
  result := self gitlogIn: gitRepoDir with: ' -1 --pretty=format:%h '.
  (result beginsWith: 'fatal:')
    ifTrue: [ ^ nil ].
  ^ result trimWhiteSpace
]

{ #category : 'smalltalk api' }
RwPrjGitTool >> gitinitIn: aDirectory with: args [
  ^ self tdGitTool gitinitIn: aDirectory with: args
]

{ #category : 'smalltalk api' }
RwPrjGitTool >> gitlogIn: gitRepoDir with: args [
  ^ self tdGitTool gitlogIn: gitRepoDir with: args
]

{ #category : 'smalltalk api' }
RwPrjGitTool >> gitlogtool: commitish limit: limit gitRepoDirectory: gitRepoDirectory [
  ^ self tdGitTool
    gitlogIn: gitRepoDirectory
    with:
      '--date=relative --format="%h %cd %s" -' , limit printString , ' ' , commitish
]

{ #category : 'smalltalk api' }
RwPrjGitTool >> gitPresentIn: gitRepoDir [
  | gitHome command cdResponse |
  [ 
  gitHome := self gitrevparseShowTopLevelIn: gitRepoDir.
  command := 'set -e; cd ' , gitRepoDir fullName , '; pwd'.
  cdResponse := self performOnServer: command logging: true ]
    on: Error
    do: [ :ex | ^ false ].
  ^ gitHome = cdResponse
]

{ #category : 'smalltalk api' }
RwPrjGitTool >> gitpullIn: gitRepoDir with: args [
  ^ self tdGitTool gitpullIn: gitRepoDir with: args
]

{ #category : 'smalltalk api' }
RwPrjGitTool >> gitpushIn: gitRepoDir with: args [
  ^ self tdGitTool gitpushIn: gitRepoDir with: args
]

{ #category : 'smalltalk api' }
RwPrjGitTool >> gitrevparseShowTopLevelIn: aDirectory [
  ^ self tdGitTool gitrevparseShowTopLevelIn: aDirectory
]

{ #category : 'smalltalk api' }
RwPrjGitTool >> performOnServer: command logging: logging [
  ^ self tdGitTool performOnServer: command logging: logging
]

{ #category : 'accessing' }
RwPrjGitTool >> tdGitTool [
  ^ tdGitTool ifNil: [ tdGitTool := TDGitTool new ]
]
