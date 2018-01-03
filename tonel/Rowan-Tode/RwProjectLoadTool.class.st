Class {
	#name : 'RwProjectLoadTool',
	#superclass : 'RwAbstractLoadTool',
	#category : 'Rowan-Tode'
}

{ #category : 'man page' }
RwProjectLoadTool >> manPage [
	^TDManPage
		commandName: 'load'
    	sourceMethod: self class name asString , '>>manPage'
    	fromString:
'NAME
  rowan project load - xxx
SYNOPSIS
  rowan project load [-h|--help]
DESCRIPTION
EXAMPLES
  ./rowan project load --help
  ./rowan project load -h
'
]
