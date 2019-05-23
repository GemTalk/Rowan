rowan support
createRwFiletreeRepositoryForPath: repositoryDirectoryPath

	^ CypressFileSystemRepository new
		initializeDefaultRepositoryProperties;
		initializeForDirectory: repositoryDirectoryPath;
		codeFormatProperty: 'FileTree';
		strictCodeFormat: false;
		initializeReaderAndWriterClasses;
		url: self printString;
		yourself