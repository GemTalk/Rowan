Class {
	#name : 'RwRepCommitTool',
	#superclass : 'RwRepositoryTool',
	#category : 'Rowan-Tode'
}

{ #category : 'smalltalk api' }
RwRepCommitTool >> commitRepositoryForSpecification: aRwSpecification message: messageString [
  self specification: aRwSpecification.
  specification canCommit
    ifFalse: [ 
      | msg |
      msg := 'repository for ' , specification specName printString
        ,
          ' does not support commit operations. Source written to repository and skipping commit'.
      self inform: msg.
      ^ msg ].
  specification commitForTool: self message: messageString.
  ^ specification updateLoadedCommitIdForTool: self
]

{ #category : 'smalltalk api' }
RwRepCommitTool >> commitRepositoryForSpecUrl: aSpecUrlString message: messageString [
  "commit repository"

  ^ self
    commitRepositoryForSpecification: (RwSpecification fromUrl: aSpecUrlString)
    message: messageString
]

{ #category : 'git' }
RwRepCommitTool >> doGitCommit: messageString [
  | gitTool gitRootDir commitMessageFileName status loadedCommitId |
  gitTool := self class git.
  gitRootDir := ServerFileDirectory
    on: specification repoSpec repositoryRootPath.
  commitMessageFileName := gitTool createTmpFileWith: messageString.
  gitTool gitaddIn: gitRootDir with: '.'.
  gitTool gitcommitIn: gitRootDir with: '-a --file=' , commitMessageFileName.
  status := gitTool gitlogIn: gitRootDir with: '-1'.
  Transcript
    cr;
    show: '==============';
    cr;
    show: status.
  ^ status
]

{ #category : 'man page' }
RwRepCommitTool >> manPage [
  ^ TDManPage
    commandName: 'xxx'
    sourceMethod: self class name asString , '>>manPage'
    fromString:
      'NAME
  rep xxx - xxx
SYNOPSIS
  rep xxx
DESCRIPTION
EXAMPLES
  rep xxx 
'
]
