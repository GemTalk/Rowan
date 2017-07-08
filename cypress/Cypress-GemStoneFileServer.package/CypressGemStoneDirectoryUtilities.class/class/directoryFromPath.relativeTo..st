"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: utilities
"
directoryFromPath: directoryPath relativeTo: aDirectory

	^((aDirectory endsWith: self pathNameDelimiter) or: [directoryPath beginsWith: self pathNameDelimiter])
		ifTrue: [aDirectory, directoryPath]
		ifFalse: [aDirectory, self pathNameDelimiter, directoryPath]
