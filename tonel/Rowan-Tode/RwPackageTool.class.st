Class {
	#name : 'RwPackageTool',
	#superclass : 'RwTool',
	#category : 'Rowan-Tode'
}

{ #category : 'commands' }
RwPackageTool class >> load [

	^RwPackageLoadTool new
]

{ #category : 'commands' }
RwPackageTool >> load [

	^self class load
]

{ #category : 'man page' }
RwPackageTool >> manPage [
	| manPage |
	manPage := TDManPage
		commandName: 'package'
		sourceMethod: self class name asString , '>>manPage'
    	fromString:
'NAME
  rowan package - Package management subcommand
SYNOPSIS
  rowan package [-h|--help]
DESCRIPTION

  Commands:
    load      Load packages for named load spec

  Use `rowan package --help <command>` to read about a specific subcommand.

OPTIONS
  --help [<subCommand>]
    Bring up this man page or a man page for a subCommand.

EXAMPLES
  ./rowan package --help
  ./rowan package -h
'.
	manPage subCommands
		at: 'clone' put: self class load manPage;
		yourself.
	^manPage
]
