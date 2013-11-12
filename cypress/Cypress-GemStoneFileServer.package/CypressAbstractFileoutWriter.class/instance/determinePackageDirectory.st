"
notice: Copyright 2013 GemTalk Systems, LLC. Open source under the MIT license.
category: private
"
determinePackageDirectory

	^self fileUtils ensureDirectoryExists: self repository directoryPath
