Class {
	#name : 'RwClassTool',
	#superclass : 'RwAbstractTool',
	#category : 'Rowan-Tools'
}

{ #category : 'commands' }
RwClassTool class >> clas [
  ^ RwClsDiffTool new
]

{ #category : 'man page' }
RwClassTool >> manPage [
  "TDRowanTool install"

  | manPage |
  manPage := TDManPage
    commandName: 'clas'
    sourceMethod: self class name asString , '>>manPage'
    fromString:
      'NAME
  clas - Rowan Class support.

SYNOPSIS
  clas [--help] <command> [<args>]

DESCRIPTION

  Commands:
    diff       Compare classes
 
  Use `clas --help <command>` to read about a specific subcommand.

OPTIONS
  --help [<subCommand>]
    Bring up this man page or a man page for a subCommand.


EXAMPLES
  clas --help

'.
  manPage subCommands
    at: 'clas' put: self class clas manPage;
    yourself.
  ^ manPage
]
