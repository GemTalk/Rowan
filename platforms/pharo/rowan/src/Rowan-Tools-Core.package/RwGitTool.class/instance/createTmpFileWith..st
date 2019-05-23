smalltalk api
createTmpFileWith: fileContents

	| file filename |
	filename := (self performOnServer: '/bin/mktemp --tmpdir commitMessage.XXXX' logging: true) trimRight.
	[ 
	| count |
	file := GsFile openWriteOnServer: filename.
	(count := file nextPutAll: fileContents withGemstoneLineEndings)
		ifNil: [ self error: 'failed write' ] ]
		ensure: [ file close ].
	^ filename