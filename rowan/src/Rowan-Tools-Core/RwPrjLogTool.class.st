Class {
	#name : 'RwPrjLogTool',
	#superclass : 'RwProjectTool',
	#category : 'Rowan-Tools-Core'
}

{ #category : 'smalltalk api' }
RwPrjLogTool >> commitLogProjectNamed: projectName limit: logLimit [

	^ (Rowan image loadedProjectNamed: projectName) commitLog: logLimit
]

{ #category : 'git' }
RwPrjLogTool >> doGitCommitLog: logLimit [

	| gitTool gitRootPath |
	gitTool := Rowan gitTools.
	gitRootPath := specification repoSpec repositoryRootPath.
	^ gitTool gitlogtool: 'HEAD' limit: logLimit gitRepoDirectory: gitRootPath
]
