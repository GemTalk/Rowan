Class {
	#name : 'FileSpec',
	#superclass : 'Object',
	#instVars : [
		'accessMode',
		'encoding',
		'missingFileAction',
		'fileType',
		'compressionPolicy'
	],
	#category : 'FileSystem-Disk-Base'
}

{ #category : 'instance creation' }
FileSpec class >> append [

	^self basicNew
		initializeForAppend;
		yourself
]

{ #category : 'instance creation' }
FileSpec class >> appendAndRead [

	^self basicNew
		initializeForAppendAndRead;
		yourself
]

{ #category : 'instance creation' }
FileSpec class >> appendAndReadBinary [

	^self basicNew
		initializeForAppendAndReadBinary;
		yourself
]

{ #category : 'instance creation' }
FileSpec class >> appendBinary [

	^self basicNew
		initializeForAppendBinary;
		yourself
]

{ #category : 'instance creation' }
FileSpec class >> read [

	^self basicNew
		initializeForRead;
		yourself
]

{ #category : 'instance creation' }
FileSpec class >> readBinary [

	^self basicNew
		initializeForReadBinary;
		yourself
]

{ #category : 'instance creation' }
FileSpec class >> readWrite [

	^self basicNew
		initializeForReadWrite;
		yourself
]

{ #category : 'instance creation' }
FileSpec class >> readWriteBinary [

	^self basicNew
		initializeForReadWriteBinary;
		yourself
]

{ #category : 'instance creation' }
FileSpec class >> readWriteTruncated [

	^self basicNew
		initializeForReadWriteTruncated;
		yourself
]

{ #category : 'instance creation' }
FileSpec class >> readWriteTruncatedBinary [

	^self basicNew
		initializeForReadWriteTruncatedBinary;
		yourself
]

{ #category : 'instance creation' }
FileSpec class >> write [

	^self basicNew
		initializeForWrite;
		yourself
]

{ #category : 'instance creation' }
FileSpec class >> writeBinary [

	^self basicNew
		initializeForWriteBinary;
		yourself
]

{ #category : 'instance creation' }
FileSpec class >> writeHighZipped [

	^self basicNew
		initializeForWriteHighZipped;
		yourself
]

{ #category : 'instance creation' }
FileSpec class >> writeLowZipped [

	^self basicNew
		initializeForWriteLowZipped;
		yourself
]

{ #category : 'instance creation' }
FileSpec class >> writeZipped [

	^self basicNew
		initializeForWriteZipped;
		yourself
]

{ #category : 'accessing' }
FileSpec >> compressionPolicy [
	^compressionPolicy
]

{ #category : 'accessing' }
FileSpec >> compressionPolicy: anObject [
	anObject fileSpec: self.
	compressionPolicy := anObject
]

{ #category : 'accessing' }
FileSpec >> encoding [
	^encoding
]

{ #category : 'accessing' }
FileSpec >> encoding: anObject [
	encoding := anObject
]

{ #category : 'accessing' }
FileSpec >> fileType [
	^fileType
]

{ #category : 'accessing' }
FileSpec >> fileType: aFileType [
	aFileType fileSpec: self.
	fileType := aFileType
]

{ #category : 'initialization' }
FileSpec >> initialize [
	self fileType: FileTextTypeOption new.
	self compressionPolicy: NoCompression new
]

{ #category : 'initialization' }
FileSpec >> initializeForAppend [
	self initialize.
	self openMode: #FileMode append.
	self missingFileAction: #MissingFileAction new.
	self encoding: #'utf8'
]

{ #category : 'initialization' }
FileSpec >> initializeForAppendAndRead [
	self initialize.
	self openMode: #FileMode appendAndRead.
	self missingFileAction: #MissingFileAction new.
	self encoding: #'utf8'
]

{ #category : 'initialization' }
FileSpec >> initializeForAppendAndReadBinary [
	self initializeForAppendAndRead.
	self fileType:  FileBinaryTypeOption new
]

{ #category : 'initialization' }
FileSpec >> initializeForAppendBinary [
	self initializeForAppend.
	self fileType: FileBinaryTypeOption new
]

{ #category : 'initialization' }
FileSpec >> initializeForRead [
	self initialize.
	self openMode: #FileMode read.
	self missingFileAction: #MissingFileAction new.
	self encoding: #'utf8'
]

{ #category : 'initialization' }
FileSpec >> initializeForReadBinary [
	self initializeForRead.
	self fileType: FileBinaryTypeOption new
]

{ #category : 'initialization' }
FileSpec >> initializeForReadWrite [
	self initialize.
	self openMode: #FileMode readWrite.
	self missingFileAction: #MissingFileAction new.
	self encoding: #'utf8'
]

{ #category : 'initialization' }
FileSpec >> initializeForReadWriteBinary [
	self initializeForReadWrite.
	self fileType: FileBinaryTypeOption new
]

{ #category : 'initialization' }
FileSpec >> initializeForReadWriteTruncated [
	self initialize.
	self openMode: #FileMode readWriteTruncated.
	self missingFileAction: #MissingFileAction new.
	self encoding: #'utf8'
]

{ #category : 'initialization' }
FileSpec >> initializeForReadWriteTruncatedBinary [
	self initializeForReadWriteTruncated.
	self fileType: FileBinaryTypeOption new
]

{ #category : 'initialization' }
FileSpec >> initializeForWrite [
	self initialize.
	self openMode: #FileMode write.
	self missingFileAction: #CreateFileAction new.
	self encoding: #'utf8'
]

{ #category : 'initialization' }
FileSpec >> initializeForWriteBinary [
	self initializeForWrite.
	self fileType: FileBinaryTypeOption new
]

{ #category : 'initialization' }
FileSpec >> initializeForWriteHighZipped [
	self initializeForWrite.
	self compressionPolicy: GZip high
]

{ #category : 'initialization' }
FileSpec >> initializeForWriteLowZipped [
	self initializeForWrite.
	self compressionPolicy: GZip low
]

{ #category : 'initialization' }
FileSpec >> initializeForWriteZipped [
	self initializeForWrite.
	self compressionPolicy: GZip none
]

{ #category : 'testing' }
FileSpec >> isWritable [

	^self openMode isWritable
]

{ #category : 'accessing' }
FileSpec >> missingFileAction [
	^missingFileAction
]

{ #category : 'accessing' }
FileSpec >> missingFileAction: anObject [
	missingFileAction := anObject
]

{ #category : 'accessing' }
FileSpec >> openMode [
	^accessMode
]

{ #category : 'accessing' }
FileSpec >> openMode: anObject [
	anObject fileSpec: self.
	accessMode := anObject
]

{ #category : 'converting' }
FileSpec >> openModeString [
	| aStream |
	aStream := WriteStream on: String new.
	self compressionPolicy printOpenModeStringOn: aStream.
	^aStream contents
]

{ #category : 'prototype' }
FileSpec >> writeStreamFor: aFileReference [

	^ ZnCharacterWriteStream
		on: (ZnBufferedWriteStream on: (aFileReference fileSystem binaryWriteStreamOn: aFileReference path))
		encoding: self encoding asString
]
