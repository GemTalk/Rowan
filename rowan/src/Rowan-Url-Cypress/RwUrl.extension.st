Extension { #name : 'RwUrl' }

{ #category : '*rowan-url-cypress' }
RwUrl >> createRwCypressRepositoryForPath: repositoryDirectoryPath [
  | repo |
  repo := CypressFileSystemRepository new
    initializeDefaultRepositoryProperties;
    codeFormatProperty: 'Cypress';
    strictCodeFormat: false;
    initializeForDirectory: repositoryDirectoryPath;
    url: self printString;
    yourself.
  ^ repo
]

{ #category : '*rowan-url-cypress' }
RwUrl >> createRwFiletreeRepositoryForPath: repositoryDirectoryPath [

	^ CypressFileSystemRepository new
		initializeDefaultRepositoryProperties;
		initializeForDirectory: repositoryDirectoryPath;
		codeFormatProperty: 'FileTree';
		strictCodeFormat: false;
		initializeReaderAndWriterClasses;
		url: self printString;
		yourself
]

{ #category : '*rowan-url-cypress' }
RwUrl >> createRwTonelRepositoryForPath: repositoryDirectoryPath [
  ^ CypressTonelRepository new
    initializeDefaultRepositoryProperties;
    initializeForDirectory: repositoryDirectoryPath;
    codeFormatProperty: 'Tonel';
    strictCodeFormat: false;
    initializeReaderAndWriterClasses;
    url: self printString;
    yourself
]
