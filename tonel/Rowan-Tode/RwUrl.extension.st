Extension { #name : 'RwUrl' }

{ #category : '*rowan-tode' }
RwUrl >> asRwRepository [
  "return a platform appropriate repository for the repository identified in the receiver"

  ^ self subclassResponsibility: #'asRwRepository'
]

{ #category : '*rowan-tode' }
RwUrl >> asRwUrl [
  ^ self
]

{ #category : '*rowan-tode' }
RwUrl >> asZnUrl [
  ^ ZnUrl fromString: self printString
]

{ #category : '*rowan-tode' }
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

{ #category : '*rowan-tode' }
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

{ #category : '*rowan-tode' }
RwUrl >> createRwRepositoryForFormat: repositoryFormat forPath: repositoryDirectoryPath [
  repositoryFormat = 'tonel'
    ifTrue: [ ^ self createRwTonelRepositoryForPath: repositoryDirectoryPath ].
  repositoryFormat = 'filetree'
    ifTrue: [ ^ self createRwFiletreeRepositoryForPath: repositoryDirectoryPath ].
  repositoryFormat = 'cypress'
    ifTrue: [ ^ self createRwCypressRepositoryForPath: repositoryDirectoryPath ]
]

{ #category : '*rowan-tode' }
RwUrl >> createRwRepositoryForPath: repositoryDirectoryPath [
  | hasCypress hasFiletree repositoryFormat |
  hasCypress := hasFiletree := false.
  (self fileUtils directoryEntriesFrom: repositoryDirectoryPath)
    do: [ :entry | 
      | filename |
      filename := self fileUtils localNameFrom: entry.
      filename = '.cypress'
        ifTrue: [ hasCypress := true ].
      filename = '.filetree'
        ifTrue: [ hasFiletree := true ] ].
  hasCypress
    ifTrue: [ 
      self fileUtils
        readStreamFor: '.cypress'
        in: repositoryDirectoryPath
        do: [ :fileStream | 
          repositoryFormat := (STON fromStream: fileStream)
            at: #'format'
            ifAbsent: [ 'filetree' ] ].
      ^ self
        createRwRepositoryForFormat: repositoryFormat
        forPath: repositoryDirectoryPath ].
  hasFiletree
    ifTrue: [ ^ self createRwRepositoryForFormat: 'filetree' forPath: repositoryDirectoryPath ].
  ^ self createRwRepositoryForFormat: 'cypress' forPath: repositoryDirectoryPath
]

{ #category : '*rowan-tode' }
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

{ #category : '*rowan-tode' }
RwUrl >> fileUtils [
  ^ Rowan fileUtilities
]

{ #category : '*rowan-tode' }
RwUrl class >> httpFromString: aString [
  ^ ZnUrl fromString: aString
]
