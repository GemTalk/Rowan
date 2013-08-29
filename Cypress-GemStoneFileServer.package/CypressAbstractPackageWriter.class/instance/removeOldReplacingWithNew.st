writing
removeOldReplacingWithNew

	self fileUtils deleteAll: self packageDirectory.
	self writePropertiesFile.
	self writePackageStructure