smalltalk api
gitaddIn: gitRepoPath with: args

	^ self performGitCommand: 'add' in: gitRepoPath with: args