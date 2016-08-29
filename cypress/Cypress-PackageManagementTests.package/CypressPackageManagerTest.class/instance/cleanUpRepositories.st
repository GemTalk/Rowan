"
notice: Copyright 2013 GemTalk Systems, LLC. Open source under the MIT license.
category: set up / teardown
"
cleanUpRepositories

	repositoriesToCleanUp reverseDo: 
			[:each |
			CypressFileUtilities current deleteAll: each directoryPath.
			GsFile removeServerDirectory: each directoryPath]