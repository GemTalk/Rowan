Class {
	#name : 'FileSpec',
	#superclass : 'Object',
	#instVars : [
		'fileOptions',
		'encoding',
		'store'
	],
	#category : 'FileSystem-Disk-Base'
}

{ #category : 'instance creation' }
FileSpec class >> append [

	^self new
		initializeForAppend;
		yourself
]

{ #category : 'instance creation' }
FileSpec class >> appendAndRead [

	^self new
		initializeForAppendAndRead;
		yourself
]

{ #category : 'instance creation' }
FileSpec class >> appendAndReadBinary [

	^self new
		initializeForAppendAndReadBinary;
		yourself
]

{ #category : 'instance creation' }
FileSpec class >> appendBinary [

	^self new
		initializeForAppendBinary;
		yourself
]

{ #category : 'instance creation' }
FileSpec class >> new [
	^self on: FileSystem disk
]

{ #category : 'instance creation' }
FileSpec class >> on: aFileSystem [
	"Default is to be on the local disk filesystem."
	^self basicNew
		initializeOn: aFileSystem;
		yourself
]

{ #category : 'instance creation' }
FileSpec class >> read [
	^self new
		initializeForRead;
		yourself
]

{ #category : 'instance creation' }
FileSpec class >> readBinary [

	^self new
		initializeForReadBinary;
		yourself
]

{ #category : 'instance creation' }
FileSpec class >> readWrite [

	^self new
		initializeForReadWrite;
		yourself
]

{ #category : 'instance creation' }
FileSpec class >> readWriteBinary [

	^self new
		initializeForReadWriteBinary;
		yourself
]

{ #category : 'instance creation' }
FileSpec class >> readWriteTruncated [

	^self new
		initializeForReadWriteTruncated;
		yourself
]

{ #category : 'instance creation' }
FileSpec class >> readWriteTruncatedBinary [

	^self new
		initializeForReadWriteTruncatedBinary;
		yourself
]

{ #category : 'instance creation' }
FileSpec class >> write [

	^self new
		initializeForWrite;
		yourself
]

{ #category : 'instance creation' }
FileSpec class >> writeBinary [

	^self new
		initializeForWriteBinary;
		yourself
]

{ #category : 'instance creation' }
FileSpec class >> writeHighZipped [

	^self new
		initializeForWriteHighZipped;
		yourself
]

{ #category : 'instance creation' }
FileSpec class >> writeLowZipped [

	^self new
		initializeForWriteLowZipped;
		yourself
]

{ #category : 'instance creation' }
FileSpec class >> writeZipped [

	^self new
		initializeForWriteZipped;
		yourself
]

{ #category : 'convenience' }
FileSpec >> beNonEncoded [
	self encoding: 'null'
]

{ #category : 'convenience' }
FileSpec >> beUnixStore [
	self store: UnixStore createDefault
]

{ #category : 'convenience' }
FileSpec >> beUtf16Encoded [
	self encoding: 'utf16'
]

{ #category : 'convenience' }
FileSpec >> beUtf32Encoded [
	self encoding: 'utf32'
]

{ #category : 'convenience' }
FileSpec >> beUtf8Encoded [
	self encoding: 'utf8'
]

{ #category : 'convenience' }
FileSpec >> beWindowsStore [
	self store: WindowsStore createDefault
]

{ #category : 'stream creation' }
FileSpec >> binaryFileStreamOn: aFileReference [

	^(aFileReference fileSystem open: aFileReference options: self fileOptions) binaryStream
]

{ #category : 'convenience' }
FileSpec >> bufferedStreamClass [
	^self isWritable 
		ifTrue: [ZnBufferedWriteStream] 
		ifFalse: [ZnBufferedReadStream]
]

{ #category : 'stream creation' }
FileSpec >> bufferedStreamFor: aFileReference [

	^ self bufferedStreamClass on: (self binaryFileStreamOn: aFileReference)
]

{ #category : 'convenience' }
FileSpec >> characterStreamClass [
	^self isWritable 
		ifTrue: [ZnCharacterWriteStream] 
		ifFalse: [ZnCharacterReadStream]
]

{ #category : 'stream creation' }
FileSpec >> encodedStreamFor: aFileReference [

	^ self characterStreamClass
		on: (self bufferedStreamClass on: (self binaryFileStreamOn: aFileReference))
		encoding: self encoding
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

{ #category : 'convenience' }
FileSpec >> fileType [
	^self fileOptions fileType
]

{ #category : 'initialization' }
FileSpec >> initialize [
	self beNonEncoded
]

{ #category : 'initialization' }
FileSpec >> initializeForAppend [
	self fileOptions: FileOptions append.
	self beUtf8Encoded
]

{ #category : 'initialization' }
FileSpec >> initializeForAppendAndRead [
	self fileOptions: FileOptions append read.
	self beUtf8Encoded
]

{ #category : 'initialization' }
FileSpec >> initializeForAppendAndReadBinary [
	self fileOptions: FileOptions append read binaryFileType.
	self beUtf8Encoded
]

{ #category : 'initialization' }
FileSpec >> initializeForAppendBinary [
	self fileOptions: FileOptions append binaryFileType.
	self beUtf8Encoded
]

{ #category : 'initialization' }
FileSpec >> initializeForRead [
	self fileOptions: FileOptions read.
	self beUtf8Encoded
]

{ #category : 'initialization' }
FileSpec >> initializeForReadBinary [
	self fileOptions: FileOptions read binaryFileType.
	self beUtf8Encoded
]

{ #category : 'initialization' }
FileSpec >> initializeForReadWrite [
	self fileOptions: FileOptions readWrite.
	self beUtf8Encoded
]

{ #category : 'initialization' }
FileSpec >> initializeForReadWriteBinary [
	self fileOptions: FileOptions readWrite binaryFileType.
	self beUtf8Encoded
]

{ #category : 'initialization' }
FileSpec >> initializeForReadWriteTruncated [
	self fileOptions: FileOptions readWrite truncate create.
	self beUtf8Encoded
]

{ #category : 'initialization' }
FileSpec >> initializeForReadWriteTruncatedBinary [
	self fileOptions: FileOptions readWrite truncate binaryFileType.
	self beUtf8Encoded
]

{ #category : 'initialization' }
FileSpec >> initializeForWrite [
	self fileOptions: FileOptions write.
	self beUtf8Encoded
]

{ #category : 'initialization' }
FileSpec >> initializeForWriteBinary [
	self fileOptions: FileOptions write binaryFileType.
	self beUtf8Encoded
]

{ #category : 'initialization' }
FileSpec >> initializeForWriteHighZipped [
	self fileOptions: FileOptions write gzipHighCompression
]

{ #category : 'initialization' }
FileSpec >> initializeForWriteLowZipped [
	self fileOptions: FileOptions write gzipLowCompression
]

{ #category : 'initialization' }
FileSpec >> initializeForWriteZipped [
	self fileOptions: FileOptions write.
	"self fileOptions fileType: FileGzipTypeFlag none"
]

{ #category : 'initialization' }
FileSpec >> initializeOn: aFileSystem [
	self initialize.
	self store: aFileSystem store
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
FileSpec >> mode [
	^self fileOptions mode
]

{ #category : 'accessing' }
FileSpec >> store [
	^store
]

{ #category : 'accessing' }
FileSpec >> store: anObject [
	store := anObject
]
