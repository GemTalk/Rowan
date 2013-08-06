"
notice: This work is protected by copyright. All rights reserved.
category: utilities
"
readStreamFor: filePath in: aDirectory do: aOneArgBlock

	self
		readStreamFor: (self directoryFromPath: filePath relativeTo: aDirectory)
		do: aOneArgBlock