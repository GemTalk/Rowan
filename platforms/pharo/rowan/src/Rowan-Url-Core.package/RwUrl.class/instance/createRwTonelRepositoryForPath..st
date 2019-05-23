rowan support
createRwTonelRepositoryForPath: repositoryDirectoryPath
  ^ CypressTonelRepository new
    initializeDefaultRepositoryProperties;
    initializeForDirectory: repositoryDirectoryPath;
    codeFormatProperty: 'Tonel';
    strictCodeFormat: false;
    initializeReaderAndWriterClasses;
    url: self printString;
    yourself