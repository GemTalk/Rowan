git
doGitPush: remote

	| gitTool gitRootPath |
	gitTool := Rowan gitTools.
	gitRootPath := specification repoSpec repositoryRootPath.
	^ remote isEmpty
		ifTrue: [ gitTool gitpushIn: gitRootPath with: '' ]
		ifFalse: [ 
			| currentBranchName |
			currentBranchName := gitTool gitBranchNameIn: gitRootPath.
			gitTool gitpushIn: gitRootPath with: remote , ' ' , currentBranchName ]