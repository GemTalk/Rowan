"
No class-specific documentation for CypGemStoneFileUtilities, hierarchy is: 
Object
  CypFileUtilities
    CypGemStoneFileUtilities

"
Class {
	#name : 'RwGsFileUtilities',
	#superclass : 'RwFileUtilities',
	#category : 'Rowan-GemStone-Core'
}

{ #category : 'private' }
RwGsFileUtilities class >> _endsWithSpecial: filename [
  "Answer true if the given filename ends with any of the special sequences
	'/..' '/.' '\..' '\.', false otherwise."

  | filenameSize finalChars |
  filenameSize := filename size.
  finalChars := filename copyFrom: filenameSize - 1 to: filenameSize.
  finalChars = '/.'
    ifTrue: [ ^ true ].
  finalChars = '\.'
    ifTrue: [ ^ true ].
  finalChars := filename copyFrom: filenameSize - 2 to: filenameSize.
  finalChars = '/..'
    ifTrue: [ ^ true ].
  finalChars = '\..'
    ifTrue: [ ^ true ].
  ^ false
]

{ #category : 'utilities' }
RwGsFileUtilities class >> deleteAll: aDirectory [
  "Delete all the files and directories under the named directory.
	 Ensure we don't try to recursively delete . or .."

  self deleteAll: aDirectory rejecting: [ :filename | false ]

]

{ #category : 'utilities' }
RwGsFileUtilities class >> deleteAll: aDirectory rejecting: rejectBlock [
  "Delete all the files and directories under the named directory.
       Reject file and directores in aDirectory that are rejected by rejectBlock.
       The rejectBlock is not used recursively.
       Ensure we don't try to recursively delete . or .."

  | filename isFile |
  (GsFile contentsAndTypesOfDirectory: aDirectory onClient: false)
    doWithIndex: [ :each :index | 
      index odd
        ifTrue: [ filename := each ]
        ifFalse: [ 
          isFile := each.
          isFile
            ifTrue: [ 
              (rejectBlock value: filename)
                ifFalse: [ 
                  (rejectBlock value: filename)
                    ifFalse: [ GsFile removeServerFile: filename ] ] ]
            ifFalse: [ 
              (self _endsWithSpecial: filename)
                ifFalse: [ 
                  (rejectBlock value: filename)
                    ifFalse: [ 
                      self deleteAll: filename rejecting: rejectBlock.
                      GsFile removeServerDirectory: filename ] ] ] ] ]
]

{ #category : 'utilities' }
RwGsFileUtilities class >> deleteDirectory: aDirectory [
  "Delete the named directory and all of it contents"

	self deleteAll: aDirectory.
   GsFile removeServerDirectory: aDirectory

]

{ #category : 'utilities' }
RwGsFileUtilities class >> directoryEntriesFrom: aDirectory [
	"Answer fully qualified paths to the contents of aDirectory."

	^(GsFile contentsOfDirectory: aDirectory onClient: false) ifNil: [#()]
]

{ #category : 'utilities' }
RwGsFileUtilities class >> directoryExists: aDirectory [

	"handle the case where GsFile class>>existsOnServer: returns nil"
	^ (GsFile existsOnServer: aDirectory) == true
]

{ #category : 'utilities' }
RwGsFileUtilities class >> directoryFromPath: directoryPath relativeTo: aDirectory [

	^((aDirectory endsWith: self pathNameDelimiter)
		or: [directoryPath beginsWith: self pathNameDelimiter])
			ifTrue: [aDirectory , directoryPath]
			ifFalse: [aDirectory , self pathNameDelimiter , directoryPath]
]

{ #category : 'private' }
RwGsFileUtilities class >> endsWithSpecial: filename [
	"Answer true if the given filename ends with any of the special sequences
	'/..' '/.' '\..' '\.', false otherwise."

	| filenameSize finalChars |
	filenameSize := filename size.
	finalChars := filename copyFrom: filenameSize - 1 to: filenameSize.
	finalChars = '/.' ifTrue: [^true].
	finalChars = '\.' ifTrue: [^true].
	finalChars := filename copyFrom: filenameSize - 2 to: filenameSize.
	finalChars = '/..' ifTrue: [^true].
	finalChars = '\..' ifTrue: [^true].
	^false

]

{ #category : 'utilities' }
RwGsFileUtilities class >> ensureDirectoryExists: aDirectory [

	| lastSeparator |
	(GsFile existsOnServer: aDirectory) == true ifTrue: [^aDirectory].
	(GsFile createServerDirectory: aDirectory) ifNotNil: [^aDirectory].
	lastSeparator := aDirectory findLastSubString: self pathNameDelimiter
				startingAt: aDirectory size.
	lastSeparator <= 1 ifTrue: [self error: 'Cannot create directory'].
	self ensureDirectoryExists: (aDirectory copyFrom: 1 to: lastSeparator - 1).
	self ensureDirectoryExists: aDirectory
]

{ #category : 'utilities' }
RwGsFileUtilities class >> entryNamesFrom: aDirectory [
  "Answer just the name of the contents of aDirectory."

  ^ (((self directoryEntriesFrom: aDirectory)
    collect: [ :each | self localNameFrom: each ])
    reject: [ :each | each = '.' or: [ each = '..' ] ])
    sortWithBlock: [ :a :b | a <= b ]

]

{ #category : 'utilities' }
RwGsFileUtilities class >> localNameFrom: aDirectory [

	| endOfPath |
	endOfPath := aDirectory findLastSubString: self pathNameDelimiter
				startingAt: aDirectory size.
	^aDirectory copyFrom: endOfPath + 1 to: aDirectory size
]

{ #category : 'utilities' }
RwGsFileUtilities class >> pathNameDelimiter [

	^'/'
]

{ #category : 'utilities' }
RwGsFileUtilities class >> readStreamFor: filePath do: aOneArgBlock [

	| file stream result |
	GsFile serverErrorString.
	file := GsFile openReadOnServer: filePath.
	GsFile serverErrorString
		ifNotNil: [:errorMessage | self error: errorMessage].
	
	[ stream := ReadStreamPortable
				on: (file contents decodeFromUTF8ToString) .
	  result := aOneArgBlock value: stream ]
			ensure: [file close].
	^result
]

{ #category : 'utilities' }
RwGsFileUtilities class >> readStreamFor: filePath in: aDirectory do: aOneArgBlock [

	self
		readStreamFor: (self directoryFromPath: filePath relativeTo: aDirectory)
		do: aOneArgBlock
]

{ #category : 'utilities' }
RwGsFileUtilities class >> writeStreamFor: filePath in: aDirectory do: aOneArgBlock [

	| file stream |
	GsFile serverErrorString.
	file := GsFile openWriteOnServer: (self directoryFromPath: filePath relativeTo: aDirectory).
	GsFile serverErrorString ifNotNil: [:errorMessage | self error: errorMessage].
	stream := WriteStreamPortable on: String new.
	[aOneArgBlock value: stream] ensure: [file nextPutAll: stream contents encodeAsUTF8; close]

]
