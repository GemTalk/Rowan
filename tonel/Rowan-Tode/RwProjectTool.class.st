Class {
	#name : 'RwProjectTool',
	#superclass : 'RwAbstractTool',
	#instVars : [
		'specification'
	],
	#category : 'Rowan-Tode'
}

{ #category : 'commands' }
RwProjectTool class >> checkout [

	^RwPrjCheckoutTool new
]

{ #category : 'commands' }
RwProjectTool class >> clone [

	^RwPrjCloneTool new
]

{ #category : 'commands' }
RwProjectTool class >> commit [

	^RwPrjCommitTool new
]

{ #category : 'commands' }
RwProjectTool class >> diff [

	^RwPrjDiffTool new
]

{ #category : 'commands' }
RwProjectTool class >> git [
  ^ RwPrjGitTool new
]

{ #category : 'commands' }
RwProjectTool class >> list [

	^RwPrjListTool new
]

{ #category : 'commands' }
RwProjectTool class >> load [

	^RwPrjLoadTool new
]

{ #category : 'commands' }
RwProjectTool class >> log [

	^RwPrjLogTool new
]

{ #category : 'commands' }
RwProjectTool class >> pull [

	^RwPrjPullTool new
]

{ #category : 'commands' }
RwProjectTool class >> push [

	^RwPrjPushTool new
]

{ #category : 'commands' }
RwProjectTool class >> read [
  ^ RwPrjReadTool new
]

{ #category : 'commands' }
RwProjectTool class >> register [
  ^ RwPrjRegisterTool new
]

{ #category : 'commands' }
RwProjectTool class >> registry [

	^RwPrjRegistryTool new
]

{ #category : 'commands' }
RwProjectTool class >> revert [

	^RwPrjRevertTool new
]

{ #category : 'commands' }
RwProjectTool class >> spec [
  ^ RwPrjSpecTool new
]

{ #category : 'commands' }
RwProjectTool class >> test [

	^RwPrjTestTool new
]

{ #category : 'commands' }
RwProjectTool class >> unregister [

	^RwPrjUnregisterTool new
]

{ #category : 'commands' }
RwProjectTool class >> write [

	^RwPrjWriteTool new
]

{ #category : 'git' }
RwProjectTool >> doGitUpdateLoadedCommitId [
  | gitRootDir |
  gitRootDir := ServerFileDirectory
    on: specification repoSpec repositoryRootPath.
  ^ self doGitUpdateLoadedCommitId: gitRootDir
]

{ #category : 'git' }
RwProjectTool >> doGitUpdateLoadedCommitId: gitRootDir [
  | gitTool loadedCommitId |
  gitTool := self class git.
  loadedCommitId := gitTool gitcommitShaIn: gitRootDir.
  ^ specification imageSpec loadedCommitId: loadedCommitId
]

{ #category : 'smalltalk api' }
RwProjectTool >> help [

	^self manPage asText
]

{ #category : 'man page' }
RwProjectTool >> manPage [
  "TDRowanPrjTool install"

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
    commit     Save image changes to repository and commit (git repos only)
    diff       Browse package level diffs for the given spec
    list       List registered specs
    load       Load from the repository
    log        Browse commit log for the repository
    pull       Execute a `git pull` for the repository
    push       Execute a `git push` for the repository
    read       Read packages from repository
    register   Register the specification
    registry   Inspect the spec registry
    revert     Reload dirty packages from repository
    spec       Manipulate spec object
    test       Bring up test browser on tests in loaded packages
    unregister Unregister the specification
    write      Write packages to repository

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
    at: 'diff' put: self class diff manPage;
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

{ #category : 'smalltalk api' }
RwProjectTool >> packageNames [
  ^ specification packageNamesForTool: self
]

{ #category : 'operations for package specs' }
RwProjectTool >> packageNamesForPackageSpec: aPackageSpec [
  ^ aPackageSpec packageNames
]

{ #category : 'operations for project specs' }
RwProjectTool >> packageNamesForProjectSpec: aProjectSpec [
  self halt: 'not yet implemented'
]

{ #category : 'smalltalk api' }
RwProjectTool >> repositoryUrl [
  ^ self specification repositoryUrl
]

{ #category : 'smalltalk api' }
RwProjectTool >> specification: aRwSpecification [
  self validate: aRwSpecification.
  ^ specification := aRwSpecification
]

{ #category : 'smalltalk api' }
RwProjectTool >> specUrl: aString [
  ^ self specification: (RwSpecification fromUrl: aString)
]

{ #category : 'validation' }
RwProjectTool >> validate: aRwSpecification [
  aRwSpecification specName
    ifNil: [ self error: 'The specification must have a specName specified.' ]
]

{ #category : 'validation' }
RwProjectTool >> validateForRead: aRwSpecification [
  aRwSpecification repositoryRootPath
    ifNil: [ 
      self
        error:
          'The repositoryRootPath must be defined in the specification: '
            , aRwSpecification specName printString ]
]

{ #category : 'validation' }
RwProjectTool >> validateForWrite: aRwSpecification [
  aRwSpecification repositoryRootPath
    ifNil: [ 
      self
        error:
          'The repositoryRootPath must be defined in the specification: '
            , aRwSpecification specName printString ]
]
