Class {
	#name : 'RwPrjLogTool',
	#superclass : 'RwProjectTool',
	#category : 'Rowan-Tools'
}

{ #category : 'smalltalk api' }
RwPrjLogTool >> commitLogSpecification: aRwSpecification limit: logLimit [
  self specification: aRwSpecification.
  ^ specification commitLogForTool: self limit: logLimit
]

{ #category : 'smalltalk api' }
RwPrjLogTool >> commitLogSpecUrl: aString limit: logLimit [
  ^ self
    commitLogSpecification: (RwSpecification fromUrl: aString)
    limit: logLimit
]

{ #category : 'git' }
RwPrjLogTool >> doGitCommitLog: logLimit [
  | gitTool gitRootDir |
  gitTool := self class git.
  gitRootDir := ServerFileDirectory
    on: specification repoSpec repositoryRootPath.
  ^ gitTool gitlogtool: 'HEAD' limit: logLimit gitRepoDirectory: gitRootDir
]
