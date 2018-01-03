Class {
	#name : 'RwUnregisterTool',
	#superclass : 'RwTool',
	#category : 'Rowan-Tode'
}

{ #category : 'man page' }
RwUnregisterTool >> manPage [
	^TDManPage
		commandName: 'xxx'
    	sourceMethod: self class name asString , '>>xxx'
    	fromString:
'NAME
  rowan xxx - xxx
SYNOPSIS
  rowan xxx [-h|--help]
DESCRIPTION
EXAMPLES
  ./rowan xxx --help
  ./rowan xxx -h
'
]
