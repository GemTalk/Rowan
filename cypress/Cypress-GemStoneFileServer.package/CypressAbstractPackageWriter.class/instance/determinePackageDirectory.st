"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: private
"
determinePackageDirectory

	^self fileUtils ensureDirectoryExists: (self fileUtils
				directoryFromPath: self packageStructure name
				relativeTo: self repository directoryPath)