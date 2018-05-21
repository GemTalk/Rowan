Extension { #name : 'RwPrjRegistryTool' }

{ #category : '*rowan-tode' }
RwPrjRegistryTool >> manPage [
  ^ TDManPage
    commandName: 'registry'
    sourceMethod: self class name asString , '>>manPage'
    fromString:
      'NAME
  prj registry - Inspect the spec registry
SYNOPSIS
  prj registry [--names|--list]
DESCRIPTION
EXAMPLES
  prj registry          # return registry dictionary
  prj registry --list   # return an array of registered specs
  prj registry --names  # return an array of registered spec names

  prj registry;         edit
  prj registry --list;  edit
  prj registry --names; edit
'
]
