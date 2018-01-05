Class {
	#name : 'RwPullTool',
	#superclass : 'RwTool',
	#category : 'Rowan-Tode'
}

{ #category : 'git' }
RwPullTool >> doGitPull: remote [
  | gitTool gitRootDir |
  gitTool := self class git.
  gitRootDir := ServerFileDirectory
    on: specification repoSpec repositoryRootPath.
  ^ remote isEmpty
    ifTrue: [ gitTool gitpullIn: gitRootDir with: '' ]
    ifFalse: [ 
      | currentBranchName |
      currentBranchName := gitTool gitBranchNameIn: gitRootDir.
      gitTool gitpullIn: gitRootDir with: remote , ' ' , currentBranchName ]
]

{ #category : 'man page' }
RwPullTool >> manPage [
  ^ TDManPage
    commandName: 'pull'
    sourceMethod: self class name asString , '>>manPage'
    fromString:
      'NAME
  rowan pull - Execute a `git pull` for the repository
SYNOPSIS
  rowan pull [--remote=<git-remote-name>] <spec-url>
DESCRIPTION
EXAMPLES
  rowan pull Rowan
  rowan pull --remote=origin Rowan
'
]

{ #category : 'smalltalk api' }
RwPullTool >> pullSpecification: aRwSpecification [
  ^ self pullSpecification: aRwSpecification remote: nil
]

{ #category : 'smalltalk api' }
RwPullTool >> pullSpecification: aRwSpecification remote: remoteOrNil [
  | remote |
  self specification: aRwSpecification.
  remoteOrNil
    ifNil: [ remote := specification repoSpec remote ]
    ifNotNil: [ remote := remoteOrNil ].
  ^ specification pullForTool: self remote: remote
]

{ #category : 'smalltalk api' }
RwPullTool >> pullSpecUrl: aSpecUrlString [
  ^ self
    pullSpecification: (RwSpecification fromUrl: aSpecUrlString)
    remote: nil
]

{ #category : 'smalltalk api' }
RwPullTool >> pullSpecUrl: aSpecUrlString remote: remote [
  ^ self
    pullSpecification: (RwSpecification fromUrl: aSpecUrlString)
    remote: remote
]
