Extension { #name : 'RwPrjReflectionTool' }

{ #category : '*rowan-tode' }
RwPrjReflectionTool >> manPage [

	^ TDManPage
		commandName: 'reflect'
		sourceMethod: self class name asString , '>>manPage'
		fromString:
			'NAME
  prj reflect - Reflect on the classes and methods in a project
SYNOPSIS
  prj reflect ???
DESCRIPTION
EXAMPLES
'
]
