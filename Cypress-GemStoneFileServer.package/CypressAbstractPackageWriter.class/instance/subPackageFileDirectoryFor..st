"
notice: This work is protected by copyright. All rights reserved.
category: private
"
subPackageFileDirectoryFor: directoryNameOrPath

	| dir |
	dir := self fileUtils directoryFromPath: directoryNameOrPath
				relativeTo: self packageDirectory.
	self fileUtils ensureDirectoryExists: dir.
	^dir
