smalltalk api
gitinitIn: dirPath with: args

	| command |
	command := 'set -e; cd ' , dirPath , '; git init ' , args.
	^ self performOnServer: command logging: true