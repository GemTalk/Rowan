smalltalk api
gitcheckoutIn: gitRepoPath with: args

	^ self performGitCommand: 'checkout' in: gitRepoPath pathString with: args