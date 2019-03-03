Class {
	#name : 'FileSpecTest',
	#superclass : 'TestCase',
	#category : 'FileSystem-Tests-Disk-Base'
}

{ #category : 'tests' }
FileSpecTest >> testAppend [

	| aFileSpec |
	aFileSpec := StreamSpec append textFileType.
	self assert: aFileSpec mode = 'a'
]

{ #category : 'tests' }
FileSpecTest >> testAppendAndRead [

	| aStreamSpec |
	aStreamSpec := StreamSpec append read.
	self assert: aStreamSpec mode = 'a+'
]

{ #category : 'tests' }
FileSpecTest >> testAppendAndReadBinary [

	| aStreamSpec |
	aStreamSpec := StreamSpec append read binaryFileType.
	self assert: (aStreamSpec fileType isKindOf: FileBinaryTypeFlag).
	self assert: aStreamSpec mode = 'a+b'
]

{ #category : 'tests' }
FileSpecTest >> testAppendBinary [

	| aStreamSpec |
	aStreamSpec := StreamSpec append binaryFileType.
	self assert: aStreamSpec mode = 'ab'
]

{ #category : 'tests' }
FileSpecTest >> testCreation [

	| aStreamSpec |
	aStreamSpec := StreamSpec append.
	self assert: (aStreamSpec fileOptions isKindOf: FileOptions).
	self assert: aStreamSpec fileOptions parent == aStreamSpec
]

{ #category : 'tests' }
FileSpecTest >> testRead [

	| aStreamSpec |
	aStreamSpec := StreamSpec read.
	self assert: aStreamSpec mode = 'r'
]

{ #category : 'tests' }
FileSpecTest >> testReadBinary [

	| aStreamSpec |
	aStreamSpec := StreamSpec read binaryFileType.
	self assert: (aStreamSpec fileOptions fileType isKindOf: FileBinaryTypeFlag).
	self assert: aStreamSpec mode = 'rb'
]

{ #category : 'tests' }
FileSpecTest >> testReadWrite [

	| aStreamSpec |
	aStreamSpec := StreamSpec readWrite.
	self assert: aStreamSpec mode = 'r+'
]

{ #category : 'tests' }
FileSpecTest >> testReadWriteBinary [

	| aStreamSpec |
	aStreamSpec := StreamSpec readWrite binaryFileType.
	self assert: (aStreamSpec fileType isKindOf: FileBinaryTypeFlag).
	self assert: aStreamSpec mode = 'r+b'
]

{ #category : 'tests' }
FileSpecTest >> testReadWriteTruncated [

	| aStreamSpec |
	aStreamSpec := StreamSpec readWrite truncate.
	self assert: aStreamSpec mode = 'w+'
]

{ #category : 'tests' }
FileSpecTest >> testReadWriteTruncatedBinary [

	| aStreamSpec |
	aStreamSpec := StreamSpec readWrite truncate binaryFileType.
	self assert: aStreamSpec mode = 'w+b'
]

{ #category : 'tests' }
FileSpecTest >> testWrite [

	| aStreamSpec |
	aStreamSpec := StreamSpec write.
	self assert: aStreamSpec mode = 'w'
]

{ #category : 'tests' }
FileSpecTest >> testWriteBinary [

	| aStreamSpec |
	aStreamSpec := StreamSpec write binaryFileType.
	self assert: (aStreamSpec fileType isKindOf: FileBinaryTypeFlag).
	self assert: aStreamSpec mode = 'wb'
]

{ #category : 'tests' }
FileSpecTest >> testWriteHighZipped [

	| aStreamSpec |
	aStreamSpec := StreamSpec write gzipHighCompression.
	self assert: aStreamSpec isGzipped.
	self assert: aStreamSpec mode = 'w9'
]

{ #category : 'tests' }
FileSpecTest >> testWriteLowZipped [

	| aStreamSpec |
	aStreamSpec := StreamSpec write gzipLowCompression.
	self assert: aStreamSpec isGzipped.
	self assert: aStreamSpec mode = 'w1'
]

{ #category : 'tests' }
FileSpecTest >> testWriteZippedWithoutCompressionLevel [

	| aStreamSpec | 
	aStreamSpec := StreamSpec write.
	aStreamSpec fileOptions fileType: FileGzipTypeFlag none.
	self deny: aStreamSpec isGzipped.
	self assert: aStreamSpec mode = 'w'
]
