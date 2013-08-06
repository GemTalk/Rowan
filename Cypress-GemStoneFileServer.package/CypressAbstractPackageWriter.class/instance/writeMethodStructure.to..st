"
notice: This work is protected by copyright. All rights reserved.
category: writing
"
writeMethodStructure: methodStructure to: methodPath

	| filename |
	filename := self fileNameForSelector: methodStructure selector.
	self
		writeInDirectoryName: methodPath
		fileName: filename
		extension: '.st'
		visit: [:fileStream | self writeMethodStructure: methodStructure onStream: fileStream]