Extension { #name : 'RwPackageTool' }

{ #category : '*rowan-tode' }
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
    edit       Edit definitions (add/remove) in package
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
		at: 'edit' put: self class edit manPage;
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
