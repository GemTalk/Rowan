set up / teardown
cleanUpRepositories

	repositoriesToCleanUp reverseDo: 
			[:each |
			CypressFileUtilities current deleteAll: each directoryPath.
			GsFile removeServerDirectory: each directoryPath]