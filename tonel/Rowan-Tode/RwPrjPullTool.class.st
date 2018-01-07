Class {
	#name : 'RwPrjPullTool',
	#superclass : 'RwProjectTool',
	#category : 'Rowan-Tode'
}

{ #category : 'git' }
RwPrjPullTool >> doGitPull: remote [
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
RwPrjPullTool >> manPage [
  ^ TDManPage
    commandName: 'pull'
    sourceMethod: self class name asString , '>>manPage'
    fromString:
      'NAME
  prj pull - Execute a `git pull` for the repository
SYNOPSIS
  prj pull [--remote=<git-remote-name>] <spec-url>
DESCRIPTION
EXAMPLES
  prj pull Rowan
  prj pull --remote=origin Rowan
'
]

{ #category : 'smalltalk api' }
RwPrjPullTool >> pullSpecification: aRwSpecification [
  ^ self pullSpecification: aRwSpecification remote: nil
]

{ #category : 'smalltalk api' }
RwPrjPullTool >> pullSpecification: aRwSpecification remote: remoteOrNil [
  | remote |
  self specification: aRwSpecification.
  remoteOrNil
    ifNil: [ remote := specification repoSpec remote ]
    ifNotNil: [ remote := remoteOrNil ].
  ^ specification pullForTool: self remote: remote
]

{ #category : 'smalltalk api' }
RwPrjPullTool >> pullSpecUrl: aSpecUrlString [
  ^ self
    pullSpecification: (RwSpecification fromUrl: aSpecUrlString)
    remote: nil
]

{ #category : 'smalltalk api' }
RwPrjPullTool >> pullSpecUrl: aSpecUrlString remote: remote [
  ^ self
    pullSpecification: (RwSpecification fromUrl: aSpecUrlString)
    remote: remote
]
