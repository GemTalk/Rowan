smalltalk api
gitcommitIn: gitRepoPath with: args

	^ self performGitCommand: 'commit' in: gitRepoPath with: args