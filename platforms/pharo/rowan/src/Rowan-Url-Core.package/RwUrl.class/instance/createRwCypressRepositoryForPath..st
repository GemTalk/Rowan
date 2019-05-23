rowan support
createRwCypressRepositoryForPath: repositoryDirectoryPath
  | repo |
  repo := CypressFileSystemRepository new
    initializeDefaultRepositoryProperties;
    codeFormatProperty: 'Cypress';
    strictCodeFormat: false;
    initializeForDirectory: repositoryDirectoryPath;
    url: self printString;
    yourself.
  ^ repo