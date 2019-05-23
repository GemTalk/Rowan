git
doGitUpdateLoadedCommitId

	| repoRootPath |
	repoRootPath := specification repoSpec repositoryRootPath.
	repoRootPath
		ifNil: [ 
			"in memory repo without a rootPath is legal"
			^ nil ].
	^ self doGitUpdateLoadedCommitId: repoRootPath