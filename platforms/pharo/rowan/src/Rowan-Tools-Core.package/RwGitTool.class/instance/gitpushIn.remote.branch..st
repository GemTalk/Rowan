smalltalk api
gitpushIn: gitRepoPath remote: remoteName branch: branchName

	^ self gitpushIn: gitRepoPath with: remoteName , ' ' , branchName