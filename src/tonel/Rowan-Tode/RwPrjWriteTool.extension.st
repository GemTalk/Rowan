Extension { #name : 'RwPrjWriteTool' }

{ #category : '*rowan-tode' }
RwPrjWriteTool >> manPage [
	^ TDManPage
		commandName: 'write'
		sourceMethod: self class name asString , '>>manPage'
		fromString:
			'NAME
  prj write - Write project to disk and commit repository
SYNOPSIS
  prj write <spec-url>
DESCRIPTION
EXAMPLES
  prj write Rowan
'
]
