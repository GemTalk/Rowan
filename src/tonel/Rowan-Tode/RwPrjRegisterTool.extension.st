Extension { #name : 'RwPrjRegisterTool' }

{ #category : '*rowan-tode' }
RwPrjRegisterTool >> manPage [
  ^ TDManPage
    commandName: 'register'
    sourceMethod: self class name asString , '>>manPage'
    fromString:
      'NAME
  prj register - Register the specification
SYNOPSIS
  prj register [--name=<spec-name>] <spec-url>
DESCRIPTION
EXAMPLES
  prj register \
    https://raw.githubusercontent.com/dalehenrich/CypressReferenceImplementation/prj/rowanLoadSpec.ston
  prj register \
    file:$GS_HOME/shared/repos/rowan/CypressReferenceImplementation/rowanLoadSpec.ston

  prj register --name=Rowan \
    https://raw.githubusercontent.com/dalehenrich/CypressReferenceImplementation/rowan/rowanLoadSpec.ston
  prj register --name=Rowan \
    file:$GS_HOME/shared/repos/rowan/CypressReferenceImplementation/rowanLoadSpec.ston

  prj register --name=OldRowan Rowan
'
]
