Extension { #name : 'RwGitTool' }

{ #category : '*rowan-tools-gemstone' }
RwGitTool >> createTmpFileWith: fileContents [

	| file filename |
	filename := (self performOnServer: self _mktempCommand) trimRight.
	[ 
	| count |
	file := GsFile openWriteOnServer: filename.
	(count := file nextPutAll: fileContents withGemstoneLineEndings)
		ifNil: [ self error: 'failed write' ] ]
		ensure: [ file close ].
	^ filename
]

{ #category : '*rowan-tools-gemstone' }
RwGitTool >> performOnServer: commandLine status: statusBlock [
  | performOnServerStatusArray |
  performOnServerStatusArray := System _performOnServer: commandLine.
  ^ statusBlock value: performOnServerStatusArray
]
