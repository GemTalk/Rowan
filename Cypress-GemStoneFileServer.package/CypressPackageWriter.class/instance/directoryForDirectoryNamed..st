private
directoryForDirectoryNamed: directoryNameOrPath

	^directoryNameOrPath = '.'
		ifTrue: [self fileUtils ensureDirectoryExists: self packageDirectory]
		ifFalse: [self subPackageFileDirectoryFor: directoryNameOrPath]