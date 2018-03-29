Class {
	#name : 'RwRepPushTool',
	#superclass : 'RwRepositoryTool',
	#category : 'Rowan-Tools'
}

{ #category : 'man page' }
RwRepPushTool >> manPage [
  ^ TDManPage
    commandName: 'xxx'
    sourceMethod: self class name asString , '>>manPage'
    fromString:
      'NAME
  rep xxx - xxx
SYNOPSIS
  rep xxx
DESCRIPTION
EXAMPLES
  rep xxx 
'
]
