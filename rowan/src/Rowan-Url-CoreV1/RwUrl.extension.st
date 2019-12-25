Extension { #name : 'RwUrl' }

{ #category : '*rowan-url-corev1' }
RwUrl >> createRwRepositoryForFormat: repositoryFormat forPath: repositoryDirectoryPath [
  repositoryFormat = 'tonel'
    ifTrue: [ ^ self createRwTonelRepositoryForPath: repositoryDirectoryPath ].
  repositoryFormat = 'filetree'
    ifTrue: [ ^ self createRwFiletreeRepositoryForPath: repositoryDirectoryPath ].
  repositoryFormat = 'cypress'
    ifTrue: [ ^ self createRwCypressRepositoryForPath: repositoryDirectoryPath ]
]

{ #category : '*rowan-url-corev1' }
RwUrl >> createRwRepositoryForPath: repositoryDirectoryPath [
  | hasCypress hasFiletree hasTonel repositoryFormat |
  hasTonel := hasCypress := hasFiletree := false.
  (self fileUtils directoryEntriesFrom: repositoryDirectoryPath)
    do: [ :entry | 
      | filename |
      filename := self fileUtils localNameFrom: entry.
      filename = 'properties.st'
        ifTrue: [ hasTonel := true ].
      filename = '.cypress'
        ifTrue: [ hasCypress := true ].
      filename = '.filetree'
        ifTrue: [ hasFiletree := true ] ].
  hasCypress | hasTonel
    ifTrue: [
      | theFilename |
      theFilename := hasTonel
        ifTrue: [ 'properties.st' ]
        ifFalse: [ '.cypress' ].
      self fileUtils
        readStreamFor: theFilename
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

{ #category : '*rowan-url-corev1' }
RwUrl >> fileUtils [
  ^ Rowan fileUtilities
]
