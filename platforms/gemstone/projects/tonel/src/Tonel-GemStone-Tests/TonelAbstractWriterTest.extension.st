Extension { #name : 'TonelAbstractWriterTest' }

{ #category : '*tonel-gemstone-tests' }
TonelAbstractWriterTest >> contentsOfFileNamed: fileName inDirectory: dir [
  self fileUtils
    readStreamFor: fileName
    in: dir
    do: [ :stream | ^ stream contents ]

]

{ #category : '*tonel-gemstone-tests' }
TonelAbstractWriterTest >> directory [
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
TonelAbstractWriterTest >> directoryNamed: directoryName existsIn: dir [
  | fileUtils filePath |
  fileUtils := self fileUtils.
  filePath := fileUtils directoryFromPath: directoryName relativeTo: dir.
  ^ fileUtils directoryExists: filePath

]

{ #category : '*tonel-gemstone-tests' }
TonelAbstractWriterTest >> directoryNamed: directoryName in: dir [
  | fileUtils |
  fileUtils := self fileUtils.
  ^ fileUtils directoryFromPath: directoryName relativeTo: dir
]

{ #category : '*tonel-gemstone-tests' }
TonelAbstractWriterTest >> fileNamesIn: dir [
  | fileUtils |
  fileUtils := self fileUtils.
  ^ fileUtils entryNamesFrom: dir
]

{ #category : '*tonel-gemstone-tests' }
TonelAbstractWriterTest >> tearDown [
  directory
    ifNotNil: [ :dir | 
      | fileUtils |
      fileUtils := self fileUtils.
      (fileUtils directoryExists: dir)
        ifTrue: [ fileUtils deleteAll: dir ] ].
  super tearDown

]
