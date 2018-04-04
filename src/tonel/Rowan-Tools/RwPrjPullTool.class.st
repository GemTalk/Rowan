Class {
	#name : 'RwPrjPullTool',
	#superclass : 'RwProjectTool',
	#category : 'Rowan-Tools'
}

{ #category : 'git' }
RwPrjPullTool >> doGitPull: remote [

	| gitTool gitRootPath |
	gitTool := self class git.
	gitRootPath := specification repoSpec repositoryRootPath.
	^ remote isEmpty
		ifTrue: [ gitTool gitpullIn: gitRootPath with: '' ]
		ifFalse: [ 
			| currentBranchName |
			currentBranchName := gitTool gitBranchNameIn: gitRootPath.
			gitTool gitpullIn: gitRootPath with: remote , ' ' , currentBranchName ]
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
