git
doGitCommitLog: logLimit

	| gitTool gitRootPath |
	gitTool := Rowan gitTools.
	gitRootPath := specification repoSpec repositoryRootPath.
	^ gitTool gitlogtool: 'HEAD' limit: logLimit gitRepoDirectory: gitRootPath