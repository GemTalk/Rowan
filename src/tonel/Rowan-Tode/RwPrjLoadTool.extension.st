Extension { #name : 'RwPrjLoadTool' }

{ #category : '*rowan-tode' }
RwPrjLoadTool >> manPage [
	^ TDManPage
		commandName: 'load'
		sourceMethod: self class name asString , '>>manPage'
		fromString:
			'NAME
  prj load - Read project from repository and install (disk -> image)
SYNOPSIS
  prj load [--loads=<load-expression>] <spec-url>
DESCRIPTION
EXAMPLES

  # first download and clone the project, then load
  prj clone --gitRoot=$GS_HOME/shared/repos \
    https://raw.githubusercontent.com/dalehenrich/CypressReferenceImplementation/rowan/Rowan.ston
  prj load Rowan

  prj load Rowan										# load default packages
  prj load --loads=ALL Rowan						# load all packages
  prj load --loads=`#( ''Rowan-Tode'')` Rowan		# load Rowan-Tode package in addition 
'
]
