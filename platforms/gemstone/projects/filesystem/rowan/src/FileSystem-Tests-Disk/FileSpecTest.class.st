Class {
	#name : 'FileSpecTest',
	#superclass : 'TestCase',
	#category : 'FileSystem-Tests-Disk-Base'
}

{ #category : 'tests' }
FileSpecTest >> testAppend [

	| aFileSpec |
	aFileSpec := FileSpec append.
	self assert: (aFileSpec openMode isKindOf: #FileAppendMode).
	self assert: aFileSpec openMode fileSpec == aFileSpec.
	self assert: (aFileSpec fileType isKindOf: FileTextTypeOption).
	self assert: aFileSpec openModeString = 'a'
]

{ #category : 'tests' }
FileSpecTest >> testAppendAndRead [

	| aFileSpec |
	aFileSpec := FileSpec appendAndRead.
	self assert: (aFileSpec openMode isKindOf: #FileAppendMode).
	self assert: aFileSpec openMode fileSpec == aFileSpec.
	self assert: (aFileSpec fileType isKindOf: FileTextTypeOption).
	self assert: aFileSpec openModeString = 'a+'
]

{ #category : 'tests' }
FileSpecTest >> testAppendAndReadBinary [

	| aFileSpec |
	aFileSpec := FileSpec appendAndReadBinary.
	self assert: (aFileSpec openMode isKindOf: #FileAppendMode).
	self assert: aFileSpec openMode fileSpec == aFileSpec.
	self assert: (aFileSpec fileType isKindOf: FileBinaryTypeOption).
	self assert: aFileSpec openModeString = 'a+b'
]

{ #category : 'tests' }
FileSpecTest >> testAppendBinary [

	| aFileSpec |
	aFileSpec := FileSpec appendBinary.
	self assert: (aFileSpec openMode isKindOf: #FileAppendMode).
	self assert: aFileSpec openMode fileSpec == aFileSpec.
	self assert: (aFileSpec fileType isKindOf: FileBinaryTypeOption).
	self assert: aFileSpec openModeString = 'ab'
]

{ #category : 'tests' }
FileSpecTest >> testRead [

	| aFileSpec |
	aFileSpec := FileSpec read.
	self assert: (aFileSpec openMode isKindOf: #FileReadMode).
	self assert: aFileSpec openMode fileSpec == aFileSpec.
	self assert: (aFileSpec fileType isKindOf: FileTextTypeOption).
	self assert: aFileSpec openModeString = 'r'
]

{ #category : 'tests' }
FileSpecTest >> testReadBinary [

	| aFileSpec |
	aFileSpec := FileSpec readBinary.
	self assert: (aFileSpec openMode isKindOf: #FileReadMode).
	self assert: aFileSpec openMode fileSpec == aFileSpec.
	self assert: (aFileSpec fileType isKindOf: FileBinaryTypeOption).
	self assert: aFileSpec openModeString = 'rb'
]

{ #category : 'tests' }
FileSpecTest >> testReadWrite [

	| aFileSpec |
	aFileSpec := FileSpec readWrite.
	self assert: (aFileSpec openMode isKindOf: #FileReadWriteMode).
	self assert: aFileSpec openMode fileSpec == aFileSpec.
	self assert: (aFileSpec fileType isKindOf: FileTextTypeOption).
	self assert: aFileSpec openModeString = 'r+'
]

{ #category : 'tests' }
FileSpecTest >> testReadWriteBinary [

	| aFileSpec |
	aFileSpec := FileSpec readWriteBinary.
	self assert: (aFileSpec openMode isKindOf: #FileReadWriteMode).
	self assert: aFileSpec openMode fileSpec == aFileSpec.
	self assert: (aFileSpec fileType isKindOf: FileBinaryTypeOption).
	self assert: aFileSpec openModeString = 'r+b'
]

{ #category : 'tests' }
FileSpecTest >> testReadWriteTruncated [

	| aFileSpec |
	aFileSpec := FileSpec readWriteTruncated.
	self assert: (aFileSpec openMode isKindOf: #FileReadWriteMode).
	self assert: aFileSpec openMode fileSpec == aFileSpec.
	self assert: (aFileSpec fileType isKindOf: FileTextTypeOption).
	self assert: aFileSpec openModeString = 'w+'
]

{ #category : 'tests' }
FileSpecTest >> testReadWriteTruncatedBinary [

	| aFileSpec |
	aFileSpec := FileSpec readWriteTruncatedBinary.
	self assert: (aFileSpec openMode isKindOf: #FileReadWriteMode).
	self assert: aFileSpec openMode fileSpec == aFileSpec.
	self assert: (aFileSpec fileType isKindOf: FileBinaryTypeOption).
	self assert: aFileSpec openModeString = 'w+b'
]

{ #category : 'tests' }
FileSpecTest >> testWrite [

	| aFileSpec |
	aFileSpec := FileSpec write.
	self assert: (aFileSpec openMode isKindOf: #FileWriteMode).
	self assert: aFileSpec openMode fileSpec == aFileSpec.
	self assert: (aFileSpec fileType isKindOf: FileTextTypeOption).
	self assert: aFileSpec openModeString = 'w'
]

{ #category : 'tests' }
FileSpecTest >> testWriteBinary [

	| aFileSpec |
	aFileSpec := FileSpec writeBinary.
	self assert: (aFileSpec openMode isKindOf: #FileWriteMode).
	self assert: aFileSpec openMode fileSpec == aFileSpec.
	self assert: (aFileSpec fileType isKindOf: FileBinaryTypeOption).
	self assert: aFileSpec openModeString = 'wb'
]

{ #category : 'tests' }
FileSpecTest >> testWriteHighZipped [

	| aFileSpec |
	aFileSpec := FileSpec writeHighZipped.
	self assert: (aFileSpec openMode isKindOf: #FileWriteMode).
	self assert: aFileSpec openMode fileSpec == aFileSpec.
	self assert: (aFileSpec fileType isKindOf: FileTextTypeOption).
	self assert: (aFileSpec compressionPolicy isKindOf: GZip).
	self assert: aFileSpec openModeString = 'w9'
]

{ #category : 'tests' }
FileSpecTest >> testWriteLowZipped [

	| aFileSpec |
	aFileSpec := FileSpec writeLowZipped.
	self assert: (aFileSpec openMode isKindOf: #FileWriteMode).
	self assert: aFileSpec openMode fileSpec == aFileSpec.
	self assert: (aFileSpec fileType isKindOf: FileTextTypeOption).
	self assert: (aFileSpec compressionPolicy isKindOf: GZip).
	self assert: aFileSpec openModeString = 'w1'
]

{ #category : 'tests' }
FileSpecTest >> testWriteZipped [

	| aFileSpec |
	aFileSpec := FileSpec writeZipped.
	self assert: (aFileSpec openMode isKindOf: #FileWriteMode).
	self assert: aFileSpec openMode fileSpec == aFileSpec.
	self assert: (aFileSpec fileType isKindOf: FileTextTypeOption).
	self assert: (aFileSpec compressionPolicy isKindOf: GZip).
	self assert: aFileSpec openModeString = 'w'
]

{ #category : 'tests' }
FileSpecTest >> testWriteZippedWithoutCompressionLevel [

	| aFileSpec |
	aFileSpec := FileSpec writeZipped.
	self assert: (aFileSpec openMode isKindOf: #FileWriteMode).
	self assert: aFileSpec openMode fileSpec == aFileSpec.
	self assert: (aFileSpec fileType isKindOf: FileTextTypeOption).
	self assert: (aFileSpec compressionPolicy isKindOf: GZip).
	self assert: aFileSpec openModeString = 'w'
]
