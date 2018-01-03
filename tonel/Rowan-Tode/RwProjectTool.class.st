Class {
	#name : 'RwProjectTool',
	#superclass : 'RwTool',
	#category : 'Rowan-Tode'
}

{ #category : 'commands' }
RwProjectTool >> load [

	^RwProjectLoadTool new
]

{ #category : 'man page' }
RwProjectTool >> manPage [
	| manPage |
	manPage := TDManPage
		commandName: 'project'
		sourceMethod: self class name asString , '>>manPage'
    	fromString:
'NAME
  rowan project - Project management subcommand
SYNOPSIS
  rowan project [-h|--help]
DESCRIPTION

  Commands:
    load      Load projects for named load spec

  Use `rowan project --help <command>` to read about a specific subcommand.

OPTIONS
  --help [<subCommand>]
    Bring up this man page or a man page for a subCommand.

EXAMPLES
  ./rowan project --help
  ./rowan project -h
'.
	manPage subCommands
		at: 'clone' put: self load manPage;
		yourself.
	^manPage
]
