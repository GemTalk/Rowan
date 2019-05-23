git
doGitPull: remote

	| gitTool gitRootPath |
	gitTool := Rowan gitTools.
	gitRootPath := specification repoSpec repositoryRootPath.
	^ remote isEmpty
		ifTrue: [ gitTool gitpullIn: gitRootPath with: '' ]
		ifFalse: [ 
			| currentBranchName |
			currentBranchName := gitTool gitBranchNameIn: gitRootPath.
			gitTool gitpullIn: gitRootPath with: remote , ' ' , currentBranchName ]