Extension { #name : 'RwPrjReadTool' }

{ #category : '*rowan-tode' }
RwPrjReadTool >> manPage [
	^ TDManPage
		commandName: 'read'
		sourceMethod: self class name asString , '>>manPage'
		fromString:
			'NAME
  prj read - Read project from repository (disk -> defs)
SYNOPSIS
  prj read <spec-url>
DESCRIPTION

  Read the packages specified in the spec and return an RwPackageSet of packages.

EXAMPLES
  prj read Rowan
'
]
