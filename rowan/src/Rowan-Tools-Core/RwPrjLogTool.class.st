Class {
	#name : 'RwPrjLogTool',
	#superclass : 'RwProjectTool',
	#category : 'Rowan-Tools-Core'
}

{ #category : 'smalltalk api' }
RwPrjLogTool >> commitLogProjectNamed: projectName limit: logLimit [

	| loadedProject |
	loadedProject := Rowan image loadedProjectNamed: projectName.
	self specification: loadedProject specification.
	^ specification commitLogForTool: self limit: logLimit
]

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

	| gitTool gitRootPath |
	gitTool := Rowan gitTools.
	gitRootPath := specification repoSpec repositoryRootPath.
	^ gitTool gitlogtool: 'HEAD' limit: logLimit gitRepoDirectory: gitRootPath
]
