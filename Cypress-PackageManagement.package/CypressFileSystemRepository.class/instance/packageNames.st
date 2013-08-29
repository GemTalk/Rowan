accessing
packageNames

	^(self fileUtils directoryEntriesFrom: self directoryPath , '*', self packageExtension)
		collect: [:each | (self fileUtils localNameFrom: each) copyWithoutSuffix: self packageExtension]