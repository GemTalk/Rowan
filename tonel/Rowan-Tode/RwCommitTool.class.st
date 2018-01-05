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
  | gitTool repoUrlString repoUrl gitRepoDir commitMessageFileName status |
  gitTool := self class git.
  repoUrlString := specification repositoryUrl.
  repoUrl := CypUrl fromString: repoUrlString.
  gitRepoDir := ServerFileDirectory on: repoUrl pathString.
  commitMessageFileName := gitTool createTmpFileWith: messageString.
  gitTool gitaddIn: gitRepoDir with: '.'.
  gitTool gitcommitIn: gitRepoDir with: '-a --file=' , commitMessageFileName.
  status := gitTool gitlogIn: gitRepoDir with: '-1'.
  Transcript
    cr;
    show: '==============';
    cr;
    show: status
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
