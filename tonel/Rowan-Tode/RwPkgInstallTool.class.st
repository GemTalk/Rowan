Class {
	#name : 'RwPkgInstallTool',
	#superclass : 'RwPackageTool',
	#category : 'Rowan-Tode'
}

{ #category : 'man page' }
RwPkgInstallTool >> manPage [
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
