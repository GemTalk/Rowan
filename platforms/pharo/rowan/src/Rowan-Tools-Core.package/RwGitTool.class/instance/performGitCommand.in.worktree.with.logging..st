private
performGitCommand: gitCommand in: gitRepoPath worktree: workTreePath with: gitArgs logging: logging

	"unconditional cd to work-tree because of git pull problem with git prior to 1.7.7.2"

	| commandBase command |
	commandBase := 'set -e; cd ' , workTreePath , '; git --git-dir ' , gitRepoPath
		, '/.git --work-tree ' , workTreePath , ' ' , gitCommand , ' '.
	command := commandBase , gitArgs.
	^ self performOnServer: command logging: logging