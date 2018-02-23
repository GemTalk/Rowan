Class {
	#name : 'RwPrjRevertTool',
	#superclass : 'RwProjectTool',
	#category : 'Rowan-Tode'
}

{ #category : 'man page' }
RwPrjRevertTool >> manPage [
	^ TDManPage
		commandName: 'revert'
		sourceMethod: self class name asString , '>>manPage'
		fromString:
			'NAME
  prj revert - Read project from repository and install, ignoring dirty packages (disk -> image)
SYNOPSIS
  prj revert
DESCRIPTION
EXAMPLES
  prj revert Rowan
'
]
