Class {
	#name : 'RwCloneTool',
	#superclass : 'RwTool',
	#category : 'Rowan-Tode'
}

{ #category : 'private' }
RwCloneTool >> _validateForGitRootPathForSpecification: gitRootDir gitRepoDir: gitRepoDir useSsh: useSsh ifDone: doneBlock [
  | gitTool response command cdResponse |
  gitTool := self class git.
  specification repositoryUrl
    ifNotNil: [ :repoUrlString | 
      | repoUrl repoDir |
      repoUrl := CypUrl fromString: repoUrlString.
      repoDir := ServerFileDirectory on: repoUrl pathString.
      response := gitTool
        gitrevparseShowTopLevelIn: (ServerFileDirectory on: repoUrl pathString).
      command := 'set -e; cd ' , gitRepoDir fullName , '; pwd'.
      cdResponse := gitTool performOnServer: command logging: true.
      response = cdResponse
        ifTrue: [ 
          | msg |
          msg := 'A clone for ' , specification specName printString
            , ' already exists in ' , gitRepoDir fullName printString
            , '. Skipping clone.'.
          self inform: msg.
          doneBlock value: msg ].
      self
        error:
          'A clone for a different project (' , response printString
            , ') is already present in ' , gitRepoDir fullName printString ].
  gitRepoDir exists
    ifTrue: [ 
      [ 
      "if gitRepoDir exists and it is a git repo, then make sure that the clone is for the target
		project ... if not error out of here."
      response := gitTool gitrevparseShowTopLevelIn: gitRepoDir.
      command := 'set -e; cd ' , gitRepoDir fullName , '; pwd'.
      cdResponse := gitTool performOnServer: command logging: true.
      response = cdResponse
        ifTrue: [ 
          | msg |
          msg := 'A clone for ' , specification specName printString
            , ' already exists in ' , gitRepoDir fullName printString
            , '. Updating spec repositoryUrl and skipping clone.'.
          self inform: msg.
          specification repositoryUrl: 'filetree:' , gitRepoDir fullName , '/', specification repoPath, '/'.
          specification repoSpec repositoryRootPath: gitRepoDir fullName.
          doneBlock value: msg ] ]
        on: Error
        do: [ :ignored | 
          self
            error:
              'Expected a git repository to present in the directory: '
                , gitRepoDir fullName printString ] ].
  gitRootDir exists
    ifFalse: [ gitRootDir assureExistence ]
]

{ #category : 'smalltalk api' }
RwCloneTool >> cloneSpecification: aRwSpecification gitRootPath: gitRootPath useSsh: useSsh [
  | gitRootDir gitTool response projectUrl gitRepoDir cloneUrl cloneOption checkout |
  self specification: aRwSpecification.
  projectUrl := RwUrl fromString: specification projectUrl.
  gitRootDir := ServerFileDirectory on: gitRootPath.
  gitRepoDir := gitRootDir directoryNamed: projectUrl path last.
  self
    _validateForGitRootPathForSpecification: gitRootDir
    gitRepoDir: gitRepoDir
    useSsh: useSsh
    ifDone: [ :msg | 
      "done block is invoked when it is not necessary to perform further processing. Typically,
		the clone is already present."
      ^ msg ].
  gitTool := self class git.
  cloneUrl := useSsh
    ifTrue: [ 'git@' , projectUrl authority , ':' ]
    ifFalse: [ 'https://' , projectUrl authority , '/' ].
  cloneUrl := cloneUrl , (projectUrl path at: 1) , '/' , (projectUrl path at: 2)
    , '.git'.
  cloneOption := ' --no-checkout '.
  checkout := specification repoSpec committish.
  checkout ifNil: [ cloneOption := '' ].
  response := gitTool gitcloneIn: gitRootDir with: cloneOption , cloneUrl.
  specification
    repositoryUrl:
      'filetree:' , gitRepoDir fullName , '/' , specification repoPath , '/'.
  specification repoSpec repositoryRootPath: gitRepoDir fullName.
  checkout ifNotNil: [ RwTool checkout checkoutSpecification: specification ].
  ^ specification
]

{ #category : 'smalltalk api' }
RwCloneTool >> cloneSpecUrl: aString gitRootPath: gitRootPath useSsh: useSsh [

	^self 
		cloneSpecification: (RwSpecification fromUrl: aString) 
		gitRootPath: gitRootPath 
		useSsh: useSsh

]

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
RwCloneTool >> specification: aRwSpecification [

  ^ specification := aRwSpecification register
]
