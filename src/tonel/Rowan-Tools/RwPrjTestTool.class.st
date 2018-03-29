Class {
	#name : 'RwPrjTestTool',
	#superclass : 'RwProjectTool',
	#category : 'Rowan-Tools'
}

{ #category : 'man page' }
RwPrjTestTool >> manPage [
  ^ TDManPage
    commandName: 'xxx'
    sourceMethod: self class name asString , '>>manPage'
    fromString:
      'NAME
  prj xxx - xxx
SYNOPSIS
  prj xxx
DESCRIPTION
EXAMPLES
  prj xxx \
    https://raw.githubusercontent.com/dalehenrich/CypressReferenceImplementation/rowan/rowanLoadSpec.ston
  prj xxx \
    file:$GS_HOME/shared/repos/rowan/CypressReferenceImplementation/rowanLoadSpec.ston
  prj xxx rowanLoadSpec
'
]
