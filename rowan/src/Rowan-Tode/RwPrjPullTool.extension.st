Extension { #name : 'RwPrjPullTool' }

{ #category : '*rowan-tode' }
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
