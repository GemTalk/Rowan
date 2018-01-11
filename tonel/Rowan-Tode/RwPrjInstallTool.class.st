Class {
	#name : 'RwPrjInstallTool',
	#superclass : 'RwProjectTool',
	#category : 'Rowan-Tode'
}

{ #category : 'man page' }
RwPrjInstallTool >> manPage [
	^ TDManPage
		commandName: 'install'
		sourceMethod: self class name asString , '>>manPage'
		fromString:
			'NAME
  prj install - Install project into the image (defs -> image)
SYNOPSIS
  prj install <spec-url>
DESCRIPTION
EXAMPLES
  prj install Rowan
'
]
