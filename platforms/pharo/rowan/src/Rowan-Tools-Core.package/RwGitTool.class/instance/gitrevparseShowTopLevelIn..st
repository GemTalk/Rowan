smalltalk api
gitrevparseShowTopLevelIn: dirPath

	| command |
	command := 'set -e; cd ' , dirPath , '; git rev-parse --show-toplevel'.
	^ self performOnServer: command logging: true