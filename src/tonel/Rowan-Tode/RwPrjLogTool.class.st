Class {
	#name : 'RwPrjLogTool',
	#superclass : 'RwProjectTool',
	#category : 'Rowan-Tode'
}

{ #category : 'smalltalk api' }
RwPrjLogTool >> commitLogSpecification: aRwSpecification limit: logLimit [
  self specification: aRwSpecification.
  ^ specification commitLogForTool: self limit: logLimit
]

{ #category : 'smalltalk api' }
RwPrjLogTool >> commitLogSpecUrl: aString limit: logLimit [
  ^ self
    commitLogSpecification: (RwSpecification fromUrl: aString)
    limit: logLimit
]

{ #category : 'git' }
RwPrjLogTool >> doGitCommitLog: logLimit [
  | gitTool gitRootDir |
  gitTool := self class git.
  gitRootDir := ServerFileDirectory
    on: specification repoSpec repositoryRootPath.
  ^ gitTool gitlogtool: 'HEAD' limit: logLimit gitRepoDirectory: gitRootDir
]

{ #category : 'man page' }
RwPrjLogTool >> manPage [
  ^ TDManPage
    commandName: 'log'
    sourceMethod: self class name asString , '>>manPage'
    fromString:
      'NAME
  prj log - Browse commit log for the repository
SYNOPSIS
  prj log [--limit=<max-commit-log-entries>] <spec-url>
DESCRIPTION
  The default --limit is 100.

EXAMPLES
  prj log Rowan 
  prj log --limit=12 Rowan 
'
]
