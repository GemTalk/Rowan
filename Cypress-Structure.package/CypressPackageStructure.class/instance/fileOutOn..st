filing out
fileOutOn: aStream

	self
		fileOutPackagePreambleOn: aStream;
		fileOutClassesOn: aStream;
		fileOutExtensionsOn: aStream