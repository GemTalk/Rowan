smalltalk api
gitbranchIn: gitRepoPath with: args

	^ self performGitCommand: 'branch' in: gitRepoPath with: args