Class {
	#name : 'EncodedBufferedStreamSpecTest',
	#superclass : 'TestCase',
	#instVars : [
		'reference'
	],
	#category : 'FileSystem-Tests-Disk-Base'
}

{ #category : 'running' }
EncodedBufferedStreamSpecTest >> setUp [
	super setUp.
	reference := (FileLocator temp  / 'plonk') resolve.
]

{ #category : 'running' }
EncodedBufferedStreamSpecTest >> tearDown [
	super tearDown.
	reference ensureDelete
]

{ #category : 'tests' }
EncodedBufferedStreamSpecTest >> testAppend [

	| aStreamSpec |
	aStreamSpec := EncodedBufferedStreamSpec append textFileType.
	self assert: aStreamSpec mode = 'a'
]

{ #category : 'tests' }
EncodedBufferedStreamSpecTest >> testAppendAndRead [

	| aStreamSpec |
	aStreamSpec := EncodedBufferedStreamSpec append read.
	self assert: aStreamSpec mode = 'a+'
]

{ #category : 'tests' }
EncodedBufferedStreamSpecTest >> testAppendAndReadBinary [

	| aStreamSpec |
	aStreamSpec := EncodedBufferedStreamSpec append read binaryFileType.
	self assert: (aStreamSpec fileType isKindOf: FileBinaryTypeFlag).
	self assert: aStreamSpec mode = 'a+b'
]

{ #category : 'tests' }
EncodedBufferedStreamSpecTest >> testAppendBinary [

	| aStreamSpec |
	aStreamSpec := EncodedBufferedStreamSpec append binaryFileType.
	self assert: aStreamSpec mode = 'ab'
]

{ #category : 'tests' }
EncodedBufferedStreamSpecTest >> testCreation [

	| aStreamSpec |
	aStreamSpec := EncodedBufferedStreamSpec append.
	self assert: (aStreamSpec fileOptions isKindOf: FileOptions).
	self assert: aStreamSpec fileOptions parent == aStreamSpec
]

{ #category : 'tests' }
EncodedBufferedStreamSpecTest >> testOnDo [
	| testString fileRef |
	testString := 'ße'.
	fileRef := FileReference newTempFilePrefix: 'FileReference' suffix: 'Test'.

	[  
		EncodedBufferedStreamSpec write utf8 on: fileRef do: [ :stream | stream nextPutAll: testString ].
		EncodedBufferedStreamSpec read utf8 on: fileRef do: [: stream |
			self assert: (stream upToAll: 'e') equals: 'ß'] 
			] ensure: [ fileRef ensureDelete ].
]

{ #category : 'tests' }
EncodedBufferedStreamSpecTest >> testOnDoIfAbsent [
	| fileRef  result |

	fileRef := FileReference newTempFilePrefix: 'FileReference' suffix: 'Test'.
	fileRef createFile.

	[ 
		result := EncodedBufferedStreamSpec read 
						on: fileRef 
						do: [:stream | stream upToEnd ]
						ifAbsent: [ self signalFailure: 'Should not reach here.' ].
		self assert: result isEmpty]
		ensure: [ fileRef ensureDelete ].

	[ 
		result := EncodedBufferedStreamSpec read 
						on: fileRef 
						do: [:stream | stream upToEnd ]
						ifAbsent: ['Should reach here.' ].
		self assert: result equals: 'Should reach here.']
			ensure: [ fileRef ensureDelete ]
]

{ #category : 'tests' }
EncodedBufferedStreamSpecTest >> testOnDoIfPresent [
	| fileRef  result |

	fileRef := FileReference newTempFilePrefix: 'FileReference' suffix: 'Test'.
	fileRef createFile.

	[ 
		result := EncodedBufferedStreamSpec write 
						on: fileRef 
						do: [:stream | 'Should not reach here' ]
						ifPresent: [ 'Should reach here' ].
		self assert: result equals: 'Should reach here' ]
		ensure: [ fileRef ensureDelete ].

	[ 
		result := EncodedBufferedStreamSpec write 
						on: fileRef 
						do: [:stream | 'Should reach here' ]
						ifPresent: ['Should not reach here' ].
		self assert: result equals: 'Should reach here']
			ensure: [ fileRef ensureDelete ]
]

{ #category : 'tests' }
EncodedBufferedStreamSpecTest >> testOnOptions [
	"This tests two ways to create a stream from the spec where the FileOptions is a parameter"
	| fileRef  stream |

	fileRef := FileReference newTempFilePrefix: 'FileReference' suffix: 'Test'.

	stream := (EncodedStreamSpec onOptions: FileOptions write) on: fileRef.
	stream nextPutAll: 'test data'.
	stream close.
	stream := (EncodedStreamSpec onOptions: FileOptions read) on: fileRef.
	self assert: stream upToEnd equals: 'test data'.

	stream := (EncodedStreamSpec on: fileRef options: FileOptions write).
	stream nextPutAll: 'test data'.
	stream close.
	stream := (EncodedStreamSpec on: fileRef options: FileOptions read).
	self assert: stream upToEnd equals: 'test data'
]

{ #category : 'tests' }
EncodedBufferedStreamSpecTest >> testRead [

	| aStreamSpec |
	aStreamSpec := EncodedBufferedStreamSpec read.
	self assert: aStreamSpec mode = 'r'
]

{ #category : 'tests' }
EncodedBufferedStreamSpecTest >> testReadCreation [
	| aSpec |
	reference createFile.
	aSpec := EncodedBufferedStreamSpec read on: reference.
	self assert: aSpec encoder class name = #ZnUTF8Encoder.
	self assert: aSpec wrappedStream class name = #ZnBufferedReadStream.
	self assert: aSpec wrappedStream wrappedStream class name = #BinaryFileStream.
	self assert: aSpec wrappedStream wrappedStream file options mode equals: 'r'
]

{ #category : 'tests' }
EncodedBufferedStreamSpecTest >> testReadWrite [
	| aSpec |
	reference createFile.
	aSpec := EncodedBufferedStreamSpec readWrite on: reference.

	self assert: aSpec class name = #ZnCharacterReadWriteStream.
	"self assert: aSpec wrappedStream class name = #BinaryFileStream.
	self assert: aSpec wrappedStream file options mode equals: 'r+'."
]

{ #category : 'tests' }
EncodedBufferedStreamSpecTest >> testReadWriteBinary [

	| aStreamSpec |
	aStreamSpec := EncodedBufferedStreamSpec readWrite binaryFileType.
	self assert: (aStreamSpec fileType isKindOf: FileBinaryTypeFlag).
	self assert: aStreamSpec mode = 'r+b'
]

{ #category : 'tests' }
EncodedBufferedStreamSpecTest >> testReadWriteTruncated [

	| aStreamSpec |
	aStreamSpec := EncodedBufferedStreamSpec readWrite truncate.
	self assert: aStreamSpec mode = 'w+'
]

{ #category : 'tests' }
EncodedBufferedStreamSpecTest >> testReadWriteTruncatedBinary [

	| aStreamSpec |
	aStreamSpec := EncodedBufferedStreamSpec readWrite truncate binaryFileType.
	self assert: aStreamSpec mode = 'w+b'
]

{ #category : 'tests' }
EncodedBufferedStreamSpecTest >> testWrite [

	| aStreamSpec |
	aStreamSpec := EncodedBufferedStreamSpec write.
	self assert: aStreamSpec mode = 'w'
]

{ #category : 'tests' }
EncodedBufferedStreamSpecTest >> testWriteBinary [

	| aStreamSpec |
	aStreamSpec := EncodedBufferedStreamSpec write binaryFileType.
	self assert: (aStreamSpec fileType isKindOf: FileBinaryTypeFlag).
	self assert: aStreamSpec mode = 'wb'
]

{ #category : 'tests' }
EncodedBufferedStreamSpecTest >> testWriteCreation [
	| aSpec |
	aSpec := EncodedBufferedStreamSpec write on: reference.
	self assert: aSpec encoder class name = #ZnUTF8Encoder.
	self assert: aSpec wrappedStream class name = #ZnBufferedWriteStream.
	self assert: aSpec wrappedStream wrappedStream class name = #BinaryFileStream.
	self assert: aSpec wrappedStream wrappedStream file options mode equals: 'w'
]

{ #category : 'tests' }
EncodedBufferedStreamSpecTest >> testWriteHighZipped [

	| aStreamSpec |
	aStreamSpec := EncodedBufferedStreamSpec write gzipHighCompression.
	self assert: aStreamSpec isGzipped.
	self assert: aStreamSpec mode = 'w9'
]
