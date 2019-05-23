smalltalk api
gitpullIn: gitRepoPath remote: remoteName branch: branchName

	^ self gitpullIn: gitRepoPath with: remoteName , ' ' , branchName