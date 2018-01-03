Class {
	#name : 'RwTool',
	#superclass : 'Object',
	#category : 'Rowan-Tode'
}

{ #category : 'commands' }
RwTool class >> clone [

	^RwCloneTool new
]

{ #category : 'commands' }
RwTool class >> commit [

	^RwCommitTool new
]

{ #category : 'commands' }
RwTool class >> diff [

	^RwDiffTool new
]

{ #category : 'commands' }
RwTool class >> list [

	^RwListTool new
]

{ #category : 'commands' }
RwTool class >> load [

	^RwLoadTool new
]

{ #category : 'commands' }
RwTool class >> log [

	^RwLogTool new
]

{ #category : 'commands' }
RwTool class >> pull [

	^RwPullTool new
]

{ #category : 'commands' }
RwTool class >> push [

	^RwPushTool new
]

{ #category : 'commands' }
RwTool class >> registry [

	^RwRegistryTool new
]

{ #category : 'commands' }
RwTool class >> revert [

	^RwRevertTool new
]

{ #category : 'commands' }
RwTool class >> save [

	^RwSaveTool new
]

{ #category : 'commands' }
RwTool class >> test [

	^RwTestTool new
]

{ #category : 'commands' }
RwTool class >> unregister [

	^RwUnregisterTool new
]

{ #category : 'smalltalk api' }
RwTool >> help [

	^self manPage asText
]

{ #category : 'man page' }
RwTool >> manPage [
  | manPage |
  manPage := TDManPage
    commandName: 'rowan'
    sourceMethod: self class name asString , '>>manPage'
    fromString:
      'NAME
  rowan - Project, package and  git repository support.

SYNOPSIS
  rowan [--help] <command> [<args>]

DESCRIPTION

  Commands:
    clone      Clone repository
    commit     Save image changes to repository and commit
    diff       View code differences between image and repository
    list       List registered specs
    load       Load from the repository
    log        Browse commit log for the repository
    pull       Execute a `git pull` for the repository
    push       Execute a `git push` for the repository
    register   Register the specification
    registry   Inspect the spec registry
    revert     Reload dirty packages from repository
    save       Save image changes to repository
    test       Bring up test browser on tests in loaded packages
    unregister Unregister the specification

  Use `rowan --help <command>` to read about a specific subcommand.

OPTIONS
  --help [<subCommand>]
    Bring up this man page or a man page for a subCommand.


EXAMPLES
  rowan --help

'.
  manPage subCommands
    at: 'clone' put: self class clone manPage;
    at: 'comit' put: self class commit manPage;
    at: 'diff' put: self class diff manPage;
    at: 'list' put: self class list manPage;
    at: 'log' put: self class log manPage;
    at: 'package' put: self class package manPage;
    at: 'project' put: self class project manPage;
    at: 'pull' put: self class pull manPage;
    at: 'push' put: self class push manPage;
    at: 'registry' put: self class registry manPage;
    at: 'revert' put: self class revert manPage;
    at: 'test' put: self class test manPage;
    at: 'unregister' put: self class unregister manPage;
    yourself.
  ^ manPage

]
