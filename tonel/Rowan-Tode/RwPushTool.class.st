Class {
	#name : 'RwPushTool',
	#superclass : 'RwTool',
	#category : 'Rowan-Tode'
}

{ #category : 'git' }
RwPushTool >> doGitPush: remote [
  | gitTool gitRootDir |
  gitTool := self class git.
  gitRootDir := ServerFileDirectory
    on: specification repoSpec repositoryRootPath.
  ^ remote isEmpty
    ifTrue: [ gitTool gitpushIn: gitRootDir with: '' ]
    ifFalse: [ 
      | currentBranchName |
      currentBranchName := gitTool gitBranchNameIn: gitRootDir.
      gitTool gitpushIn: gitRootDir with: remote , ' ' , currentBranchName ]
]

{ #category : 'man page' }
RwPushTool >> manPage [
  ^ TDManPage
    commandName: 'push'
    sourceMethod: self class name asString , '>>manPage'
    fromString:
      'NAME
  rowan push - Execute a `git push` for the repository
SYNOPSIS
  rowan push [--remote=<git-remote-name>] <spec-url>
DESCRIPTION
EXAMPLES
  rowan push Rowan
  rowan push --remote=origin Rowan
'
]

{ #category : 'smalltalk api' }
RwPushTool >> pushSpecification: aRwSpecification [
  ^ self pushSpecification: aRwSpecification remote: nil
]

{ #category : 'smalltalk api' }
RwPushTool >> pushSpecification: aRwSpecification remote: remoteOrNil [
  | remote |
  self specification: aRwSpecification.
  remoteOrNil
    ifNil: [ remote := specification repoSpec remote ]
    ifNotNil: [ remote := remoteOrNil ].
  ^ specification pushForTool: self remote: remote
]

{ #category : 'smalltalk api' }
RwPushTool >> pushSpecUrl: aSpecUrlString [
  ^ self
    pushSpecification: (RwSpecification fromUrl: aSpecUrlString)
    remote: nil
]

{ #category : 'smalltalk api' }
RwPushTool >> pushSpecUrl: aSpecUrlString remote: remote [
  ^ self
    pushSpecification: (RwSpecification fromUrl: aSpecUrlString)
    remote: remote
]
