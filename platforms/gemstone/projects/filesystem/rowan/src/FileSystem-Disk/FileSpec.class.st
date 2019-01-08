Class {
	#name : 'FileSpec',
	#superclass : 'Object',
	#instVars : [
		'fileOptions',
		'encoding',
		'fileType',
		'compressionPolicy',
		'fileStore'
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

{ #category : 'convenience' }
FileSpec >> beUnixStore [
	self fileStore: UnixStore createDefault
]

{ #category : 'convenience' }
FileSpec >> beWindowsStore [
	self fileStore: WindowsStore createDefault
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
FileSpec >> fileOptions [
	^fileOptions
]

{ #category : 'accessing' }
FileSpec >> fileOptions: aFileOptions [
	aFileOptions parent: self.
	fileOptions := aFileOptions
]

{ #category : 'accessing' }
FileSpec >> fileStore [
	^fileStore
]

{ #category : 'accessing' }
FileSpec >> fileStore: anObject [
	fileStore := anObject
]

{ #category : 'convenience' }
FileSpec >> fileType [
	^self fileOptions fileType
]

{ #category : 'initialization' }
FileSpec >> initialize [
	self compressionPolicy: NoCompression new.
	self fileStore: FileSystem disk store
]

{ #category : 'initialization' }
FileSpec >> initializeForAppend [
	self initialize.
	self fileOptions: FileOptions append.
	self encoding: #'utf8'
]

{ #category : 'initialization' }
FileSpec >> initializeForAppendAndRead [
	self initialize.
	self fileOptions: FileOptions appendAndRead.
	self encoding: #'utf8'
]

{ #category : 'initialization' }
FileSpec >> initializeForAppendAndReadBinary [
	self initialize.
	self fileOptions: FileOptions appendAndReadBinary.
	self encoding: #'utf8'
]

{ #category : 'initialization' }
FileSpec >> initializeForAppendBinary [
	self initialize.
	self fileOptions: FileOptions appendBinary.
	self encoding: #'utf8'
]

{ #category : 'initialization' }
FileSpec >> initializeForRead [
	self initialize.
	self fileOptions: FileOptions read.
	self encoding: #'utf8'
]

{ #category : 'initialization' }
FileSpec >> initializeForReadBinary [
	self initialize.
	self fileOptions: FileOptions readBinary.
	self encoding: #'utf8'
]

{ #category : 'initialization' }
FileSpec >> initializeForReadWrite [
	self initialize.
	self fileOptions: FileOptions readWrite.
	self encoding: #'utf8'
]

{ #category : 'initialization' }
FileSpec >> initializeForReadWriteBinary [
	self initialize.
	self fileOptions: FileOptions readWriteBinary.
	self encoding: #'utf8'
]

{ #category : 'initialization' }
FileSpec >> initializeForReadWriteTruncated [
	self initialize.
	self fileOptions: FileOptions readWriteTruncated.
	self encoding: #'utf8'
]

{ #category : 'initialization' }
FileSpec >> initializeForReadWriteTruncatedBinary [
	self initialize.
	self fileOptions: FileOptions readWriteTruncatedBinary.
	self encoding: #'utf8'
]

{ #category : 'initialization' }
FileSpec >> initializeForWrite [
	self initialize.
	self fileOptions: FileOptions write.
	self encoding: #'utf8'
]

{ #category : 'initialization' }
FileSpec >> initializeForWriteBinary [
	self initialize.
	self fileOptions: FileOptions writeBinary.
	self encoding: #'utf8'
]

{ #category : 'initialization' }
FileSpec >> initializeForWriteHighZipped [
	self initialize.
	self fileOptions: FileOptions writeWithHighCompression.
	self encoding: #'utf8'
]

{ #category : 'initialization' }
FileSpec >> initializeForWriteLowZipped [
	self initialize.
	self fileOptions: FileOptions writeWithLowCompression.
	self encoding: #'utf8'
]

{ #category : 'initialization' }
FileSpec >> initializeForWriteZipped [
	self initialize.
	self fileOptions: FileOptions write.
	self fileOptions fileType: FileGzipTypeOption none.
	self encoding: #'utf8'
]

{ #category : 'testing' }
FileSpec >> isClientFileSystem [

	^false
]

{ #category : 'testing' }
FileSpec >> isGzipped [

	^self fileOptions isGzipped
]

{ #category : 'testing' }
FileSpec >> isReadable [

	^self fileOptions isReadable
]

{ #category : 'testing' }
FileSpec >> isWritable [

	^self fileOptions isWritable
]

{ #category : 'printing' }
FileSpec >> modeString [
	^self fileOptions modeString
]

{ #category : 'prototype' }
FileSpec >> writeStreamFor: aFileReference [

	^ ZnCharacterWriteStream
		on: (ZnBufferedWriteStream on: (aFileReference fileSystem binaryWriteStreamOn: aFileReference path))
		encoding: self encoding asString
]
