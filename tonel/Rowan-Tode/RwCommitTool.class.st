Class {
	#name : 'RwCommitTool',
	#superclass : 'RwTool',
	#category : 'Rowan-Tode'
}

{ #category : 'smalltalk api' }
RwCommitTool >> commitSpecification: aRwSpecification message: messageString [
  self specification: aRwSpecification.
  self class write writeSpecification: specification.
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
RwCommitTool >> commitSpecUrl: aSpecUrlString message: messageString [
  ^ self
    commitSpecification: (RwSpecification fromUrl: aSpecUrlString)
    message: messageString
]

{ #category : 'git' }
RwCommitTool >> doGitCommit: messageString [
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
RwCommitTool >> manPage [
  ^ TDManPage
    commandName: 'commit'
    sourceMethod: self class name asString , '>>manPage'
    fromString:
      'NAME
  rowan commit - Save image changes to repository and commit (git repos only)
SYNOPSIS
  rowan commit --message=<message-ref> [--push[=remote]] <spec-url>
DESCRIPTION
EXAMPLES
  rowan commit --message=`implement RwCommitTool` Rowan
  rowan commit Rowan

  rowan commit --push=origin Rowan
  rowan commit --push Rowan
'
]
