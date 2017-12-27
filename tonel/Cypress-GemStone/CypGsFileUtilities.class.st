"
No class-specific documentation for CypGemStoneFileUtilities, hierarchy is: 
Object
  CypFileUtilities
    CypGemStoneFileUtilities

"
Class {
	#name : 'CypGsFileUtilities',
	#superclass : 'CypFileUtilities',
	#category : 'Cypress-GemStone'
}

{ #category : 'utilities' }
CypGsFileUtilities class >> directoryEntriesFrom: aDirectory [
	"Answer fully qualified paths to the contents of aDirectory."

	^(GsFile contentsOfDirectory: aDirectory onClient: false) ifNil: [#()]
]

{ #category : 'utilities' }
CypGsFileUtilities class >> directoryExists: aDirectory [

	^GsFile existsOnServer: aDirectory

]

{ #category : 'utilities' }
CypGsFileUtilities class >> directoryFromPath: directoryPath relativeTo: aDirectory [

	^((aDirectory endsWith: self pathNameDelimiter)
		or: [directoryPath beginsWith: self pathNameDelimiter])
			ifTrue: [aDirectory , directoryPath]
			ifFalse: [aDirectory , self pathNameDelimiter , directoryPath]
]

{ #category : 'utilities' }
CypGsFileUtilities class >> ensureDirectoryExists: aDirectory [

	| lastSeparator |
	(GsFile existsOnServer: aDirectory) ifTrue: [^aDirectory].
	(GsFile createServerDirectory: aDirectory) ifNotNil: [^aDirectory].
	lastSeparator := aDirectory findLastSubString: self pathNameDelimiter
				startingAt: aDirectory size.
	lastSeparator <= 1 ifTrue: [self error: 'Cannot create directory'].
	self ensureDirectoryExists: (aDirectory copyFrom: 1 to: lastSeparator - 1).
	self ensureDirectoryExists: aDirectory
]

{ #category : 'utilities' }
CypGsFileUtilities class >> localNameFrom: aDirectory [

	| endOfPath |
	endOfPath := aDirectory findLastSubString: self pathNameDelimiter
				startingAt: aDirectory size.
	^aDirectory copyFrom: endOfPath + 1 to: aDirectory size
]

{ #category : 'utilities' }
CypGsFileUtilities class >> pathNameDelimiter [

	^'/'
]

{ #category : 'utilities' }
CypGsFileUtilities class >> readStreamFor: filePath do: aOneArgBlock [

	| file stream result |
	GsFile serverErrorString.
	file := GsFile openReadOnServer: filePath.
	GsFile serverErrorString
		ifNotNil: [:errorMessage | self error: errorMessage].
	
	[stream := ReadStreamPortable
				on: (String withAll: file contents asByteArray decodeFromUTF8).
	result := aOneArgBlock value: stream]
			ensure: [file close].
	^result
]

{ #category : 'utilities' }
CypGsFileUtilities class >> readStreamFor: filePath in: aDirectory do: aOneArgBlock [

	self
		readStreamFor: (self directoryFromPath: filePath relativeTo: aDirectory)
		do: aOneArgBlock
]
