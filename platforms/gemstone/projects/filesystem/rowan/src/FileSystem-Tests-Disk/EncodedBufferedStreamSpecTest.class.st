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
	self should: [EncodedBufferedStreamSpec readWrite on: reference] raise: Error.

	self should: [EncodedBufferedStreamSpec read write on: reference] raise: Error.

	self should: [EncodedBufferedStreamSpec write read on: reference] raise: Error
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
	self assert: aSpec wrappedStream wrappedStream file options mode equals: 'w'.

	self should: [EncodedBufferedStreamSpec readWrite on: (FileLocator temp / 'tttt') resolve] raise: Error
]

{ #category : 'tests' }
EncodedBufferedStreamSpecTest >> testWriteHighZipped [

	| aStreamSpec |
	aStreamSpec := EncodedBufferedStreamSpec write gzipHighCompression.
	self assert: aStreamSpec isGzipped.
	self assert: aStreamSpec mode = 'w9'
]
