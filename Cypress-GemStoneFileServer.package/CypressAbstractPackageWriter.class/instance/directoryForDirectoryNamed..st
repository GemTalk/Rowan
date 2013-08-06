"
notice: This work is protected by copyright. All rights reserved.
category: private
"
directoryForDirectoryNamed: directoryNameOrPath

	^directoryNameOrPath = '.'
		ifTrue: [self fileUtils ensureDirectoryExists: self packageDirectory]
		ifFalse: [self subPackageFileDirectoryFor: directoryNameOrPath]
