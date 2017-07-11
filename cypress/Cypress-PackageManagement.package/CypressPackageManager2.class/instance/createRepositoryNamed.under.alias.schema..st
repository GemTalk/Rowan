"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: Updating
"
createRepositoryNamed: aName under: aDirectory alias: aString schema: schemaName

	^CypressFileSystemRepository createOn: (CypressUrl
				absoluteFromText: schemaName
						, (CypressFileUtilities current directoryFromPath: aName
								relativeTo: aDirectory)
							, '/')
		alias: aString