Class {
	#name : 'RwLogTool',
	#superclass : 'RwTool',
	#category : 'Rowan-Tode'
}

{ #category : 'smalltalk api' }
RwLogTool >> commitLogSpecification: aRwSpecification limit: logLimit [
  self specification: aRwSpecification.
  ^ specification commitLogForTool: self limit: logLimit
]

{ #category : 'smalltalk api' }
RwLogTool >> commitLogSpecUrl: aString limit: logLimit [
  ^ self
    commitLogSpecification: (RwSpecification fromUrl: aString)
    limit: logLimit
]

{ #category : 'git' }
RwLogTool >> doGitCommitLog: logLimit [
  | gitTool gitRootDir |
  gitTool := self class git.
  gitRootDir := ServerFileDirectory
    on: specification repoSpec repositoryRootPath.
  ^ gitTool gitlogtool: 'HEAD' limit: logLimit gitRepoDirectory: gitRootDir
]

{ #category : 'man page' }
RwLogTool >> manPage [
  ^ TDManPage
    commandName: 'log'
    sourceMethod: self class name asString , '>>manPage'
    fromString:
      'NAME
  rowan log - Browse commit log for the repository
SYNOPSIS
  rowan log [--limit=<max-commit-log-entries>] <spec-url>
DESCRIPTION
  The default --limit is 100.

EXAMPLES
  rowan log Rowan 
  rowan log --limit=12 Rowan 
'
]
