Class {
	#name : 'RwPullTool',
	#superclass : 'RwTool',
	#category : 'Rowan-Tode'
}

{ #category : 'man page' }
RwPullTool >> manPage [
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
