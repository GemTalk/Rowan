private
performGitCommand: gitCommand in: gitRepoPath with: gitArgs

	^ self
		performGitCommand: gitCommand
		in: gitRepoPath
		worktree: gitRepoPath
		with: gitArgs
		logging: true