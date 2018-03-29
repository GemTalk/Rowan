Class {
	#name : 'RwPkgRecompileTool',
	#superclass : 'RwPackageTool',
	#category : 'Rowan-Tools'
}

{ #category : 'man page' }
RwPkgRecompileTool >> manPage [
  ^ TDManPage
    commandName: 'xxx'
    sourceMethod: self class name asString , '>>manPage'
    fromString:
      'NAME
  pkg xxx - xxx
SYNOPSIS
  pkg xxx
DESCRIPTION
EXAMPLES
  pkg xxx 
'
]
