smalltalk api
gitpushIn: gitRepoPath with: args

	^ self performGitCommand: 'push' in: gitRepoPath with: args