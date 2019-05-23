rowan support
createRwRepositoryForFormat: repositoryFormat forPath: repositoryDirectoryPath
  repositoryFormat = 'tonel'
    ifTrue: [ ^ self createRwTonelRepositoryForPath: repositoryDirectoryPath ].
  repositoryFormat = 'filetree'
    ifTrue: [ ^ self createRwFiletreeRepositoryForPath: repositoryDirectoryPath ].
  repositoryFormat = 'cypress'
    ifTrue: [ ^ self createRwCypressRepositoryForPath: repositoryDirectoryPath ]