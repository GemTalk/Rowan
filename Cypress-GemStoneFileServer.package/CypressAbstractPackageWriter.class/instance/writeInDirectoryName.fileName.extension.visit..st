"
notice: This work is protected by copyright. All rights reserved.
category: private
"
writeInDirectoryName: directoryNameOrPath fileName: fileName extension: ext visit: visitBlock

	| directory |
	directory := self directoryForDirectoryNamed: directoryNameOrPath.
	self fileUtils
		writeStreamFor: fileName , ext
		in: directory
		do: [:fileStream | visitBlock value: fileStream]
