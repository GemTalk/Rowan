Class {
	#name : 'RwPrjPullTool',
	#superclass : 'RwProjectTool',
	#category : 'Rowan-Tools'
}

{ #category : 'git' }
RwPrjPullTool >> doGitPull: remote [
  | gitTool gitRootDir |
  gitTool := self class git.
  gitRootDir := ServerFileDirectory
    on: specification repoSpec repositoryRootPath.
  ^ remote isEmpty
    ifTrue: [ gitTool gitpullIn: gitRootDir with: '' ]
    ifFalse: [ 
      | currentBranchName |
      currentBranchName := gitTool gitBranchNameIn: gitRootDir.
      gitTool gitpullIn: gitRootDir with: remote , ' ' , currentBranchName ]
]

{ #category : 'smalltalk api' }
RwPrjPullTool >> pullSpecification: aRwSpecification [
  ^ self pullSpecification: aRwSpecification remote: nil
]

{ #category : 'smalltalk api' }
RwPrjPullTool >> pullSpecification: aRwSpecification remote: remoteOrNil [
  | remote |
  self specification: aRwSpecification.
  remoteOrNil
    ifNil: [ remote := specification repoSpec remote ]
    ifNotNil: [ remote := remoteOrNil ].
  ^ specification pullForTool: self remote: remote
]

{ #category : 'smalltalk api' }
RwPrjPullTool >> pullSpecUrl: aSpecUrlString [
  ^ self
    pullSpecification: (RwSpecification fromUrl: aSpecUrlString)
    remote: nil
]

{ #category : 'smalltalk api' }
RwPrjPullTool >> pullSpecUrl: aSpecUrlString remote: remote [
  ^ self
    pullSpecification: (RwSpecification fromUrl: aSpecUrlString)
    remote: remote
]
