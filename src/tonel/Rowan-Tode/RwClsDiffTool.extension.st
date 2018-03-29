Extension { #name : 'RwClsDiffTool' }

{ #category : '*rowan-tode' }
RwClsDiffTool >> manPage [
  "TDRowanTool install"

  ^ TDManPage
    commandName: 'class'
    sourceMethod: self class name asString , '>>manPage'
    fromString:
      'NAME
  class diff - Compare two classes
SYNOPSIS
  clas diff class1 class2
DESCRIPTION
EXAMPLES
  clas diff RwGsPatchSet RwGsPatchSetBasic
'
]
