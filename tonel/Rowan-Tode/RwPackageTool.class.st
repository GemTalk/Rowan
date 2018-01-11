Class {
	#name : 'RwPackageTool',
	#superclass : 'RwAbstractTool',
	#category : 'Rowan-Tode'
}

{ #category : 'commands' }
RwPackageTool class >> commit [
  ^ RwPkgCommitTool new
]

{ #category : 'commands' }
RwPackageTool class >> copy [
  ^ RwPkgCopyTool new
]

{ #category : 'commands' }
RwPackageTool class >> create [
  ^ RwPkgCreateTool new
]

{ #category : 'commands' }
RwPackageTool class >> diff [
  ^ RwPkgDiffTool new
]

{ #category : 'commands' }
RwPackageTool class >> install [
	^ RwPkgInstallTool new
]

{ #category : 'commands' }
RwPackageTool class >> list [
  ^ RwPkgListTool new
]

{ #category : 'commands' }
RwPackageTool class >> load [
  ^ RwPkgLoadTool new
]

{ #category : 'commands' }
RwPackageTool class >> move [
  ^ RwPkgMoveTool new
]

{ #category : 'commands' }
RwPackageTool class >> read [
	^ RwPkgReadTool new
]

{ #category : 'commands' }
RwPackageTool class >> recompile [
  ^ RwPkgRecompileTool new
]

{ #category : 'commands' }
RwPackageTool class >> remove [
  ^ RwPkgRemoveTool new
]

{ #category : 'commands' }
RwPackageTool class >> rename [
  ^ RwPkgRenameTool new
]

{ #category : 'commands' }
RwPackageTool class >> revert [
  ^ RwPkgRevertTool new
]

{ #category : 'commands' }
RwPackageTool class >> write [
  ^ RwPkgWriteTool new
]

{ #category : 'man page' }
RwPackageTool >> manPage [
  "TDRowanTool install"

  | manPage |
  manPage := TDManPage
    commandName: 'pkg'
    sourceMethod: self class name asString , '>>manPage'
    fromString:
      'NAME
  pkg - Rowan Package support.

SYNOPSIS
  pkg [--help] <command> [<args>]

DESCRIPTION

  Commands:
    commit     Commit packages (git repos only)
    copy       Copy packages from one repository to another
    create     Create packages
    diff       Compare packages
    list       List registered packages
    load       Load packages
    move       Move packages
    recompile  Recompile packages
    remove     Remove the packages
    rename     Rename packages
    revert     Revert packages
    write      Write packages
 
  Use `pkg --help <command>` to read about a specific subcommand.

OPTIONS
  --help [<subCommand>]
    Bring up this man page or a man page for a subCommand.


EXAMPLES
  pkg --help

'.
  manPage subCommands
    at: 'commit' put: self class commit manPage;
    at: 'copy' put: self class copy manPage;
    at: 'create' put: self class create manPage;
    at: 'diff' put: self class diff manPage;
    at: 'list' put: self class list manPage;
    at: 'load' put: self class load manPage;
    at: 'move' put: self class move manPage;
    at: 'recompile' put: self class recompile manPage;
    at: 'remove' put: self class remove manPage;
    at: 'rename' put: self class rename manPage;
    at: 'revert' put: self class revert manPage;
    at: 'write' put: self class write manPage;
    yourself.
  ^ manPage
]
