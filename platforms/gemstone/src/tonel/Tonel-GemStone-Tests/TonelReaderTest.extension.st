Extension { #name : 'TonelReaderTest' }

{ #category : '*tonel-gemstone-tests' }
TonelReaderTest >> directory [
  directory
    ifNil: [ 
      | fileUtils |
      fileUtils := self fileUtils.
      directory := fileUtils
        directoryFromPath: 'mctest'
        relativeTo: fileUtils default.
      fileUtils ensureDirectoryExists: directory ].
  ^ directory

]

{ #category : '*tonel-gemstone-tests' }
TonelReaderTest >> tearDown [
  directory
    ifNotNil: [ :dir | 
      | fileUtils |
      fileUtils := self fileUtils.
      (fileUtils directoryExists: dir)
        ifTrue: [ fileUtils deleteAll: dir ] ].
  super tearDown

]
