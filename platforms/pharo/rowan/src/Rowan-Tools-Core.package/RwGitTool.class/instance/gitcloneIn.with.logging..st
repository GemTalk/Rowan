smalltalk api
gitcloneIn: gitRootPath with: gitArgs logging: logging

	| commandBase command gitCommand |
	gitCommand := 'clone'.
	commandBase := 'set -e; cd ' , gitRootPath , '; git ' , gitCommand , ' '.
	command := commandBase , gitArgs.
	^ self performOnServer: command logging: logging