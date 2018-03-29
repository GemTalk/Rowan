Class {
	#name : 'RwRepositoryTool',
	#superclass : 'RwAbstractTool',
	#category : 'Rowan-Tools'
}

{ #category : 'commands' }
RwRepositoryTool class >> add [
  ^ RwRepAddTool new
]

{ #category : 'commands' }
RwRepositoryTool class >> commit [
  ^ RwRepCommitTool new
]

{ #category : 'commands' }
RwRepositoryTool class >> list [
  ^ RwRepListTool new
]

{ #category : 'commands' }
RwRepositoryTool class >> packages [
  ^ RwRepPackagesTool new
]

{ #category : 'commands' }
RwRepositoryTool class >> pull [
  ^ RwRepPullTool new
]

{ #category : 'commands' }
RwRepositoryTool class >> push [
  ^ RwRepPushTool new
]

{ #category : 'commands' }
RwRepositoryTool class >> remove [
  ^ RwRepRemoveTool new
]

{ #category : 'man page' }
RwRepositoryTool >> manPage [
  "TDRowanTool install"

  | manPage |
  manPage := TDManPage
    commandName: 'rep'
    sourceMethod: self class name asString , '>>manPage'
    fromString:
      'NAME
  rep - Rowan Repository support.

SYNOPSIS
  rep [--help] <command> [<args>]

DESCRIPTION

  Commands:
    add        Add a repository and create a new project spec for the repository
    commit     Commit of repository (git repos only and nothing saved/written from image)
    list       List registered repositories
    packages   List packages available from disk in the repository
    pull       Execute a `git pull` for the repository
    push       Execute a `git push` for the repository
    remove     Remove the repository from the image registry

  Use `prj --help <command>` to read about a specific subcommand.

OPTIONS
  --help [<subCommand>]
    Bring up this man page or a man page for a subCommand.


EXAMPLES
  prj --help

'.
  manPage subCommands
    at: 'add' put: self class add manPage;
    at: 'commit' put: self class commit manPage;
    at: 'list' put: self class list manPage;
    at: 'packages' put: self class packages manPage;
    at: 'pull' put: self class pull manPage;
    at: 'push' put: self class push manPage;
    at: 'remove' put: self class remove manPage;
    yourself.
  ^ manPage
]
