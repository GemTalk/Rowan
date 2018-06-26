Extension { #name : 'RwPrjPushTool' }

{ #category : '*rowan-tode' }
RwPrjPushTool >> manPage [
  ^ TDManPage
    commandName: 'push'
    sourceMethod: self class name asString , '>>manPage'
    fromString:
      'NAME
  prj push - Execute a `git push` for theprj repository
SYNOPSIS
  prj push [--remote=<git-remote-name>] <spec-url>
DESCRIPTION
EXAMPLES
  prj push Rowan
  prj push --remote=origin Rowan
'
]
