Class {
	#name : 'RwLoadTool',
	#superclass : 'RwTool',
	#category : 'Rowan-Tode'
}

{ #category : 'man page' }
RwLoadTool >> manPage [
  ^ TDManPage
    commandName: 'load'
    sourceMethod: self class name asString , '>>manPage'
    fromString:
      'NAME
  rowan load - Load from the repository
SYNOPSIS
  rowan load [--loads=<load-expression>] <spec-url>
DESCRIPTION
EXAMPLES

  # first download and clone the project, then load
  rowan clone --gitRoot=$GS_HOME/shared/repos \
    https://raw.githubusercontent.com/dalehenrich/CypressReferenceImplementation/rowan/Rowan.ston
  rowan load Rowan

  rowan load Rowan										# load default packages
  rowan load --loads=ALL Rowan						# load all packages
  rowan load --loads=`#( ''Rowan-Tode'')` Rowan		# load Rowan-Tode package in addition 
'
]
