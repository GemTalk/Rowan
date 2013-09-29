"
notice: Copyright 2013 GemTalk Systems, LLC. Open source under the MIT license.
category: private
"
createFilesFor: aDictionary
	"aDictionay is a mapping of repository file names to their expected contents."

	aDictionary keysAndValuesDo: 
			[:aPathName :fileContents |
			| filename directoryPath file |
			filename := CypressFileUtilities current localNameFrom: aPathName.
			directoryPath := aPathName copyWithoutSuffix: filename.
			CypressFileUtilities current ensureDirectoryExists: directoryPath.
			file := GsFile openWriteOnServer: aPathName.
			[file nextPutAll: fileContents] ensure: [file close]]