Extension { #name : 'RwPrjRevertTool' }

{ #category : '*rowan-tode' }
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
