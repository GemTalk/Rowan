Class {
	#name : 'FileSpecTest',
	#superclass : 'TestCase',
	#category : 'FileSystem-Tests-Disk-Base'
}

{ #category : 'tests' }
FileSpecTest >> testAppend [

	| aFileSpec |
	aFileSpec := FileSpec append.
	self assert: aFileSpec modeString = 'a'
]

{ #category : 'tests' }
FileSpecTest >> testAppendAndRead [

	| aFileSpec |
	aFileSpec := FileSpec appendAndRead.
	self assert: aFileSpec modeString = 'a+'
]

{ #category : 'tests' }
FileSpecTest >> testAppendAndReadBinary [

	| aFileSpec |
	aFileSpec := FileSpec appendAndReadBinary.
	self assert: (aFileSpec fileType isKindOf: FileBinaryTypeOption).
	self assert: aFileSpec modeString = 'a+b'
]

{ #category : 'tests' }
FileSpecTest >> testAppendBinary [

	| aFileSpec |
	aFileSpec := FileSpec appendBinary.
	self assert: aFileSpec modeString = 'ab'
]

{ #category : 'tests' }
FileSpecTest >> testCreation [

	| aFileSpec |
	aFileSpec := FileSpec append.
	self assert: (aFileSpec fileOptions isKindOf: FileOptions).
	self assert: aFileSpec fileOptions parent == aFileSpec.
	self assert: (aFileSpec compressionPolicy isKindOf: NoCompression)
]

{ #category : 'tests' }
FileSpecTest >> testRead [

	| aFileSpec |
	aFileSpec := FileSpec read.
	self assert: aFileSpec modeString = 'r'
]

{ #category : 'tests' }
FileSpecTest >> testReadBinary [

	| aFileSpec |
	aFileSpec := FileSpec readBinary.
	self assert: (aFileSpec fileOptions fileType isKindOf: FileBinaryTypeOption).
	self assert: aFileSpec modeString = 'rb'
]

{ #category : 'tests' }
FileSpecTest >> testReadWrite [

	| aFileSpec |
	aFileSpec := FileSpec readWrite.
	self assert: aFileSpec modeString = 'r+'
]

{ #category : 'tests' }
FileSpecTest >> testReadWriteBinary [

	| aFileSpec |
	aFileSpec := FileSpec readWriteBinary.
	self assert: (aFileSpec fileType isKindOf: FileBinaryTypeOption).
	self assert: aFileSpec modeString = 'r+b'
]

{ #category : 'tests' }
FileSpecTest >> testReadWriteTruncated [

	| aFileSpec |
	aFileSpec := FileSpec readWriteTruncated.
	self assert: aFileSpec modeString = 'w+'
]

{ #category : 'tests' }
FileSpecTest >> testReadWriteTruncatedBinary [

	| aFileSpec |
	aFileSpec := FileSpec readWriteTruncatedBinary.
	self assert: aFileSpec modeString = 'w+b'
]

{ #category : 'tests' }
FileSpecTest >> testWrite [

	| aFileSpec |
	aFileSpec := FileSpec write.
	self assert: aFileSpec modeString = 'w'
]

{ #category : 'tests' }
FileSpecTest >> testWriteBinary [

	| aFileSpec |
	aFileSpec := FileSpec writeBinary.
	self assert: (aFileSpec fileType isKindOf: FileBinaryTypeOption).
	self assert: aFileSpec modeString = 'wb'
]

{ #category : 'tests' }
FileSpecTest >> testWriteHighZipped [

	| aFileSpec |
	aFileSpec := FileSpec writeHighZipped.
	self assert: (aFileSpec compressionPolicy isKindOf: GZip).
	self assert: aFileSpec modeString = 'w9'
]

{ #category : 'tests' }
FileSpecTest >> testWriteLowZipped [

	| aFileSpec |
	aFileSpec := FileSpec writeLowZipped.
	self assert: (aFileSpec compressionPolicy isKindOf: GZip).
	self assert: aFileSpec modeString = 'w1'
]

{ #category : 'tests' }
FileSpecTest >> testWriteZippedWithoutCompressionLevel [

	| aFileSpec | 
	aFileSpec := FileSpec writeZipped.
	self assert: (aFileSpec compressionPolicy isKindOf: GZip).
	self assert: aFileSpec modeString = 'w'
]
