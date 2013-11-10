"
notice: Copyright 2013 GemTalk Systems, LLC. Open source under the MIT license.
category: private
"
subPackageFileDirectoryFor: directoryNameOrPath

	| dir |
	dir := self fileUtils directoryFromPath: directoryNameOrPath
				relativeTo: self packageDirectory.
	self fileUtils ensureDirectoryExists: dir.
	^dir
