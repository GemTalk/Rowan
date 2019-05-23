smalltalk api
gitstatusIn: gitRepoPath with: args

	^ self performGitCommand: 'status' in: gitRepoPath with: args