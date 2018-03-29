Extension { #name : 'RwPrjDiffTool' }

{ #category : '*rowan-tode' }
RwPrjDiffTool >> manPage [
  ^ TDManPage
    commandName: 'diff'
    sourceMethod: self class name asString , '>>manPage'
    fromString:
      'NAME
  prj diff - Browse package level diffs for the given spec
SYNOPSIS
  prj diff <spec-url>
DESCRIPTION
EXAMPLES
  prj diff \
    https://raw.githubusercontent.com/dalehenrich/CypressReferenceImplementation/rowan/rowanLoadSpec.ston
  prj diff \
    file:$GS_HOME/shared/repos/rowan/CypressReferenceImplementation/rowanLoadSpec.ston
  prj diff Rowan
'
]
