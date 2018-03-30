Extension { #name : 'RwProjectTool' }

{ #category : '*rowan-tode' }
RwProjectTool >> manPage [
	"TDRowanTool install"

	| manPage |
	manPage := TDManPage
		commandName: 'prj'
		sourceMethod: self class name asString , '>>manPage'
		fromString:
			'NAME
  prj - Project support.

SYNOPSIS
  prj [--help] <command> [<args>]

DESCRIPTION

  Commands:
    clone      Clone project repository
    commit     Write project to disk and commit repository (image -> disk)
    create     Create a new repository and project
    diff       Browse package level diffs for the given spec
    install    Install project into the image (defs -> image)
    list       List registered specs
    load       Read project from repository and install (disk -> image)
    log        Browse commit log for the repository
    pull       Execute a `git pull` for the repository
    push       Execute a `git push` for the repository
    read       Read project from repository (disk -> defs)
    register   Register the specification
    registry   Inspect the spec registry
    revert     Read project from repository and install, ignoring dirty packages (disk -> image)
    spec       Manipulate spec object
    test       Bring up test browser on tests in loaded packages
    unregister Unregister the specification
    write      Write project to disk (image -> disk)

  Use `prj --help <command>` to read about a specific subcommand.

OPTIONS
  --help [<subCommand>]
    Bring up this man page or a man page for a subCommand.


EXAMPLES
  prj --help

'.
	manPage subCommands
		at: 'clone' put: self class clone manPage;
		at: 'commit' put: self class commit manPage;
		at: 'create' put: self class create manPage;
		at: 'diff' put: self class diff manPage;
		at: 'install' put: self class install manPage;
		at: 'list' put: self class list manPage;
		at: 'load' put: self class load manPage;
		at: 'log' put: self class log manPage;
		at: 'pull' put: self class pull manPage;
		at: 'push' put: self class push manPage;
		at: 'read' put: self class read manPage;
		at: 'register' put: self class register manPage;
		at: 'registry' put: self class registry manPage;
		at: 'revert' put: self class revert manPage;
		at: 'write' put: self class write manPage;
		at: 'spec' put: self class spec manPage;
		at: 'test' put: self class test manPage;
		at: 'unregister' put: self class unregister manPage;
		yourself.
	^ manPage
]
