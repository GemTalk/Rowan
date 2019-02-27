Class {
	#name : 'FileSpecTest',
	#superclass : 'TestCase',
	#category : 'FileSystem-Tests-Disk-Base'
}

{ #category : 'tests' }
FileSpecTest >> testAppend [

	| aFileSpec |
	aFileSpec := FileSpec append textFileType.
	self assert: aFileSpec mode = 'a'
]

{ #category : 'tests' }
FileSpecTest >> testAppendAndRead [

	| aFileSpec |
	aFileSpec := FileSpec append read.
	self assert: aFileSpec mode = 'a+'
]

{ #category : 'tests' }
FileSpecTest >> testAppendAndReadBinary [

	| aFileSpec |
	aFileSpec := FileSpec append read binaryFileType.
	self assert: (aFileSpec fileType isKindOf: FileBinaryTypeFlag).
	self assert: aFileSpec mode = 'a+b'
]

{ #category : 'tests' }
FileSpecTest >> testAppendBinary [

	| aFileSpec |
	aFileSpec := FileSpec append binaryFileType.
	self assert: aFileSpec mode = 'ab'
]

{ #category : 'tests' }
FileSpecTest >> testCreation [

	| aFileSpec |
	aFileSpec := FileSpec append.
	self assert: (aFileSpec fileOptions isKindOf: FileOptions).
	self assert: aFileSpec fileOptions parent == aFileSpec
]

{ #category : 'tests' }
FileSpecTest >> testRead [

	| aFileSpec |
	aFileSpec := FileSpec read.
	self assert: aFileSpec mode = 'r'
]

{ #category : 'tests' }
FileSpecTest >> testReadBinary [

	| aFileSpec |
	aFileSpec := FileSpec read binaryFileType.
	self assert: (aFileSpec fileOptions fileType isKindOf: FileBinaryTypeFlag).
	self assert: aFileSpec mode = 'rb'
]

{ #category : 'tests' }
FileSpecTest >> testReadWrite [

	| aFileSpec |
	aFileSpec := FileSpec readWrite.
	self assert: aFileSpec mode = 'r+'
]

{ #category : 'tests' }
FileSpecTest >> testReadWriteBinary [

	| aFileSpec |
	aFileSpec := FileSpec readWrite binaryFileType.
	self assert: (aFileSpec fileType isKindOf: FileBinaryTypeFlag).
	self assert: aFileSpec mode = 'r+b'
]

{ #category : 'tests' }
FileSpecTest >> testReadWriteTruncated [

	| aFileSpec |
	aFileSpec := FileSpec readWrite truncate.
	self assert: aFileSpec mode = 'w+'
]

{ #category : 'tests' }
FileSpecTest >> testReadWriteTruncatedBinary [

	| aFileSpec |
	aFileSpec := FileSpec readWrite truncated binaryFileType.
	self assert: aFileSpec mode = 'w+b'
]

{ #category : 'tests' }
FileSpecTest >> testWrite [

	| aFileSpec |
	aFileSpec := FileSpec write.
	self assert: aFileSpec mode = 'w'
]

{ #category : 'tests' }
FileSpecTest >> testWriteBinary [

	| aFileSpec |
	aFileSpec := FileSpec write binaryFileType.
	self assert: (aFileSpec fileType isKindOf: FileBinaryTypeFlag).
	self assert: aFileSpec mode = 'wb'
]

{ #category : 'tests' }
FileSpecTest >> testWriteHighZipped [

	| aFileSpec |
	aFileSpec := FileSpec write gzipHighCompression.
	self assert: aFileSpec isGzipped.
	self assert: aFileSpec mode = 'w9'
]

{ #category : 'tests' }
FileSpecTest >> testWriteLowZipped [

	| aFileSpec |
	aFileSpec := FileSpec write gzipLowCompression.
	self assert: aFileSpec isGzipped.
	self assert: aFileSpec mode = 'w1'
]

{ #category : 'tests' }
FileSpecTest >> testWriteZippedWithoutCompressionLevel [

	| aFileSpec | 
	aFileSpec := FileSpec write.
	aFileSpec fileOptions fileType: FileGzipTypeFlag none.
	self deny: aFileSpec isGzipped.
	self assert: aFileSpec mode = 'w'
]
