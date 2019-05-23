smalltalk api
gitpullIn: gitRepoPath with: args

	^ self performGitCommand: 'pull' in: gitRepoPath with: args