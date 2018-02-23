Class {
	#name : 'RwPkgCommitTool',
	#superclass : 'RwPackageTool',
	#category : 'Rowan-Tode'
}

{ #category : 'man page' }
RwPkgCommitTool >> manPage [
  "TDRowanTool install"

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
