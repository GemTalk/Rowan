Extension { #name : 'RwPrjUnregisterTool' }

{ #category : '*rowan-tode' }
RwPrjUnregisterTool >> manPage [
  ^ TDManPage
    commandName: 'unregister'
    sourceMethod: self class name asString , '>>manPage'
    fromString:
      'NAME
  prj unregister - xxx
SYNOPSIS
  prj unregister <spec-url>
DESCRIPTION
EXAMPLES
  prj unregister Rowan
'
]
