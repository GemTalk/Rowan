"
All Cypress classes are private to GemStone and are likely to be removed in a future release.
"
Class {
	#name : 'CypressGemStoneDirectoryUtilities',
	#superclass : 'CypressFileUtilities',
	#category : 'Cypress-GemStoneFileServer'
}

{ #category : 'utilities' }
CypressGemStoneDirectoryUtilities class >> deleteAll: aDirectory [
  "Delete all the files and directories under the named directory.
	 Ensure we don't try to recursively delete . or .."

  self deleteAll: aDirectory rejecting: [ :filename | false ]

]

{ #category : 'utilities' }
CypressGemStoneDirectoryUtilities class >> deleteAll: aDirectory rejecting: rejectBlock [
	"Delete all the files and directories under the named directory.
       Reject file and directores in aDirectory that are rejected by rejectBlock.
       The rejectBlock is not used recursively.
       Ensure we don't try to recursively delete . or .."

	| filename isFile |
	(GsFile contentsAndTypesOfDirectory: aDirectory onClient: false)
		doWithIndex: 
			[:each :index |
			index odd
				ifTrue: [filename := each]
				ifFalse: 
					[isFile := each.
					isFile
						ifTrue: 
							[(rejectBlock value: filename)
								ifFalse: 
									[(rejectBlock value: filename) ifFalse: [GsFile removeServerFile: filename]]]
						ifFalse: 
							[(self endsWithSpecial: filename)
								ifFalse: 
									[(rejectBlock value: filename)
										ifFalse: 
											[self deleteAll: filename rejecting: rejectBlock.
											GsFile removeServerDirectory: filename]]]]]

]

{ #category : 'utilities' }
CypressGemStoneDirectoryUtilities class >> directoryEntriesFrom: aDirectory [
	"Answer fully qualified paths to the contents of aDirectory."

	^(GsFile contentsOfDirectory: aDirectory onClient: false) ifNil: [#()]

]

{ #category : 'utilities' }
CypressGemStoneDirectoryUtilities class >> directoryExists: aDirectory [

	"handle the case where GsFile class>>existsOnServer: returns nil"
	^ (GsFile existsOnServer: aDirectory) == true
]

{ #category : 'utilities' }
CypressGemStoneDirectoryUtilities class >> directoryFileNamesAndContents: aDirectory [
	"Walk the directory tree starting at aDirectory and
	 answer a map of the names of the files in the tree to
	 their contents (which work best when text)."

	| map |
	map := Dictionary new.
	self directoryFileNamesAndContents: aDirectory into: map.
	^map.

]

{ #category : 'utilities' }
CypressGemStoneDirectoryUtilities class >> directoryFileNamesAndContents: aDirectory into: aDictionary [
	"Walk the directory tree starting at aDirectory and
	 answer a map of the names of the files in the tree to
	 their contents (which work best when text)."

	| filename isFile |
	(GsFile contentsAndTypesOfDirectory: aDirectory onClient: false)
		doWithIndex: 
			[:each :index |
			index odd
				ifTrue: [filename := each]
				ifFalse: 
					[isFile := each.
					isFile
						ifTrue: 
							[| file |
							file := GsFile openReadOnServer: filename.
							file isNil
								ifFalse: 
									[aDictionary at: filename put: file contents.
									file close]]
						ifFalse: 
							[(self endsWithSpecial: filename)
								ifFalse: [self directoryFileNamesAndContents: filename into: aDictionary]]]]

]

{ #category : 'utilities' }
CypressGemStoneDirectoryUtilities class >> directoryFromPath: directoryPath relativeTo: aDirectory [

	| delimiter |
	delimiter := self pathNameDelimiter.
	^(aDirectory last = delimiter last
		or: [(directoryPath indexOfSubCollection: delimiter startingAt: 1 ifAbsent: [ 0 ]) = 1])
			ifTrue: [aDirectory , directoryPath]
			ifFalse: [aDirectory , delimiter , directoryPath]

]

{ #category : 'private' }
CypressGemStoneDirectoryUtilities class >> endsWithSpecial: filename [
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
CypressGemStoneDirectoryUtilities class >> ensureDirectoryExists: aDirectory [

	| lastSeparator |
	(GsFile existsOnServer: aDirectory) == true ifTrue: [^aDirectory].
	(GsFile createServerDirectory: aDirectory) ifNotNil: [^aDirectory].
	lastSeparator := aDirectory findLastSubString: self pathNameDelimiter startingAt: aDirectory size.
	lastSeparator <= 1 ifTrue: [self error: 'Cannot create directory'].
	self ensureDirectoryExists: (aDirectory copyFrom: 1 to: lastSeparator - 1).
	self ensureDirectoryExists: aDirectory.
]

{ #category : 'initializating' }
CypressGemStoneDirectoryUtilities class >> initialize [
	"self initialize"

	self install

]

{ #category : 'utilities' }
CypressGemStoneDirectoryUtilities class >> localNameFrom: aDirectory [

	| endOfPath |
	endOfPath := aDirectory findLastSubString: self pathNameDelimiter startingAt: aDirectory size.
	^aDirectory copyFrom: endOfPath + 1 to: aDirectory size

]

{ #category : 'utilities' }
CypressGemStoneDirectoryUtilities class >> pathNameDelimiter [

	^'/'

]

{ #category : 'utilities' }
CypressGemStoneDirectoryUtilities class >> readStreamFor: filePath do: aOneArgBlock [

	| file stream blockResult |
	GsFile serverErrorString.
	file := GsFile openReadOnServer: filePath.
	GsFile serverErrorString ifNotNil: [:errorMessage | self error: errorMessage].
	[stream := ReadStreamPortable on: (String withAll: file contents asByteArray decodeFromUTF8).
	blockResult := aOneArgBlock value: stream] ensure: [file close].
	^ blockResult

]

{ #category : 'utilities' }
CypressGemStoneDirectoryUtilities class >> readStreamFor: filePath in: aDirectory do: aOneArgBlock [

	^ self
		readStreamFor: (self directoryFromPath: filePath relativeTo: aDirectory)
		do: aOneArgBlock

]

{ #category : 'utilities' }
CypressGemStoneDirectoryUtilities class >> workingDirectory [

	^System gemEnvironmentVariable: 'PWD'

]

{ #category : 'utilities' }
CypressGemStoneDirectoryUtilities class >> writeStreamFor: filePath in: aDirectory do: aOneArgBlock [

	| file stream |
	GsFile serverErrorString.
	file := GsFile openWriteOnServer: (self directoryFromPath: filePath relativeTo: aDirectory).
	GsFile serverErrorString ifNotNil: [:errorMessage | self error: errorMessage].
	stream := WriteStreamPortable on: String new.
	[aOneArgBlock value: stream] ensure: [file nextPutAll: stream contents encodeAsUTF8; close]

]
