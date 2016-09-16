"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: Updating
"
createRepositoryNamed: aName under: aDirectory alias: aString schema: schemaName

	^CypressFileSystemRepository
		createOn: (schemaName
				, (CypressFileUtilities current
					directoryFromPath: aName
						relativeTo: aDirectory)
				, '/') asUrl
		alias: aString