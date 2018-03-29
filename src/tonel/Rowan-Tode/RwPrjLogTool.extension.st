Extension { #name : 'RwPrjLogTool' }

{ #category : '*rowan-tode' }
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
