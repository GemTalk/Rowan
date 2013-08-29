Updating
createRepositoryNamed: aName under: aDirectory alias: aString schema: schemaName

	^CypressFileSystemRepository
		createOn: (schemaName
				, (CypressFileUtilities current
					directoryFromPath: aName
						relativeTo: aDirectory)
				, '/') asUrl
		alias: aString