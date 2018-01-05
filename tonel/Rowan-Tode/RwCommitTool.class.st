Class {
	#name : 'RwCommitTool',
	#superclass : 'RwTool',
	#category : 'Rowan-Tode'
}

{ #category : 'smalltalk api' }
RwCommitTool >> commitSpecification: aRwSpecification message: messageString [
  | saveTool |
  self specification: aRwSpecification.
  self class save saveSpecification: specification.
  specification canCommit
    ifFalse: [ 
      | msg |
      msg := 'repository for ' , specification specName printString
        ,
          ' does not support commit operations. Source saved to repository and skipping commit'.
      self inform: msg.
      ^ msg ].
  ^ specification commitForTool: self message: messageString
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
  loadedCommitId := gitTool gitcommitShaIn: gitRootDir.
  specification imageSpec loadedCommitId: loadedCommitId
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
  rowan commit --message=<message-ref> <spec-url>
DESCRIPTION
EXAMPLES
  rowan commit --message=`implement RwCommitTool` Rowan
'
]
