smalltalk api
gitlogIn: gitRepoPath with: args

	^ self performGitCommand: 'log' in: gitRepoPath with: args