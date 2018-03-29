Class {
	#name : 'RwPrjPushTool',
	#superclass : 'RwProjectTool',
	#category : 'Rowan-Tools'
}

{ #category : 'git' }
RwPrjPushTool >> doGitPush: remote [
  | gitTool gitRootDir |
  gitTool := self class git.
  gitRootDir := ServerFileDirectory
    on: specification repoSpec repositoryRootPath.
  ^ remote isEmpty
    ifTrue: [ gitTool gitpushIn: gitRootDir with: '' ]
    ifFalse: [ 
      | currentBranchName |
      currentBranchName := gitTool gitBranchNameIn: gitRootDir.
      gitTool gitpushIn: gitRootDir with: remote , ' ' , currentBranchName ]
]

{ #category : 'smalltalk api' }
RwPrjPushTool >> pushSpecification: aRwSpecification [
  ^ self pushSpecification: aRwSpecification remote: nil
]

{ #category : 'smalltalk api' }
RwPrjPushTool >> pushSpecification: aRwSpecification remote: remoteOrNil [
  | remote |
  self specification: aRwSpecification.
  remoteOrNil
    ifNil: [ remote := specification repoSpec remote ]
    ifNotNil: [ remote := remoteOrNil ].
  ^ specification pushForTool: self remote: remote
]

{ #category : 'smalltalk api' }
RwPrjPushTool >> pushSpecUrl: aSpecUrlString [
  ^ self
    pushSpecification: (RwSpecification fromUrl: aSpecUrlString)
    remote: nil
]

{ #category : 'smalltalk api' }
RwPrjPushTool >> pushSpecUrl: aSpecUrlString remote: remote [
  ^ self
    pushSpecification: (RwSpecification fromUrl: aSpecUrlString)
    remote: remote
]
