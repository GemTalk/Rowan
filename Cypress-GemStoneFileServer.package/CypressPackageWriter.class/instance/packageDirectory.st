accessing
packageDirectory

	packageDirectory
		ifNil: 
			[packageDirectory := self fileUtils ensureDirectoryExists: (self fileUtils
								directoryFromPath: self packageStructure name
								relativeTo: self rootDirectory)].
	^packageDirectory