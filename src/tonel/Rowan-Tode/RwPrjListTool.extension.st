Extension { #name : 'RwPrjListTool' }

{ #category : '*rowan-tode' }
RwPrjListTool >> manPage [

	^ TDManPage
		commandName: 'list'
		sourceMethod: self class name asString , '>>manPage'
		fromString:
			'NAME
  prj list - List Rowan projects
SYNOPSIS
  prj list
DESCRIPTION
  The `prj list` command brings up a window on the list of Rowan projects. 

EXAMPLES
  prj list 
'
]
