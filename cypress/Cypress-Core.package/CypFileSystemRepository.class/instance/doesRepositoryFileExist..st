"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: testing - private
"
doesRepositoryFileExist: fileName
	"Answer whether the named file exists at the repository level."

	^disk directoryExists: (disk directoryFromPath: fileName
				relativeTo: self directoryPath)