"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: utilities
"
readStreamFor: filePath in: aDirectory do: aOneArgBlock

	self
		readStreamFor: (self directoryFromPath: filePath relativeTo: aDirectory)
		do: aOneArgBlock
