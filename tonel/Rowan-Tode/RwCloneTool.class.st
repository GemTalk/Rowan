Class {
	#name : 'RwCloneTool',
	#superclass : 'RwTool',
	#category : 'Rowan-Tode'
}

{ #category : 'man page' }
RwCloneTool >> manPage [
  ^ TDManPage
    commandName: 'clone'
    sourceMethod: self class name asString , '>>manPage'
    fromString:
      'NAME
  rowan clone - Clone project repository
SYNOPSIS
  rowan clone [--https] --gitRoot=<git-root-dir> <spec-url>

DESCRIPTION
  The project specifiedin the <spec-url> is cloned using an SSH url (git@github.com)
  into the directory specified by the <git-root-dir>. The spec url repository url
  is updated to point to the cloned project directory.

  If the --https option is used then the clone is performed using an HTTPS url
  (https://github.com).

EXAMPLES
  rowan clone \
    https://raw.githubusercontent.com/dalehenrich/CypressReferenceImplementation/rowan/rowanLoadSpec.ston
  rowan clone \
    file:$GS_HOME/shared/repos/rowan/CypressReferenceImplementation/rowanLoadSpec.ston

  rowan clone --gitRoot=$GS_HOME/shared/repos Rowan
'
]

{ #category : 'smalltalk api' }
RwCloneTool >> specUrl: aString as: aSpecName [
  super specification: (RwSpecification fromUrl: aString) copy.
  specification specName: aSpecName.
  specification register
]

{ #category : 'smalltalk api' }
RwCloneTool >> specUrl: aString gitRootPath: gitRootPath useSsh: useSsh [
  | gitRootDir gitTool response projectUrl gitRepoDir |
  super specification: (RwSpecification fromUrl: aString).
  gitTool := RwTool git.
  projectUrl := specification projectUrl asUrl.
  gitRootDir := ServerFileDirectory on: gitRootPath.
  gitRepoDir := gitRootDir directoryNamed: projectUrl path last.
  gitRepoDir exists
    ifTrue: [ 
      [ 
      | command cdResponse |
      response := gitTool gitrevparseShowTopLevelIn: gitRepoDir.
      command := 'cd ' , gitRepoDir fullName , '; pwd'.
      cdResponse := gitTool performOnServer: command logging: true.
      response = cdResponse
        ifTrue: [ self halt: 'looks like the project is already cloned' ]
        ifFalse: [ self halt: 'toplevel git directory is not gitRepoDir ... go ahead and clone' ] ]
        on: Error
        do: [ :ex | self halt ] ]
    ifFalse: [  ].
  self halt
]
