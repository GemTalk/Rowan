writing
writePackageStructure: aPackageStructure

	self
		packageStructure: aPackageStructure;
		packageDirectory: (self fileUtils ensureDirectoryExists: (self fileUtils
							directoryFromPath: self packageStructure name
							relativeTo: self repository directoryPath)).
	self fileUtils deleteAll: self packageDirectory.
	self writePropertiesFile.
	self writePackageStructure
