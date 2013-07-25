writing
write

	(self fileUtils directoryExists: self packageDirectory)
		ifTrue: [self fileUtils deleteAll: self packageDirectory].
	self writePropertiesFile.
	self writePackageStructure