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

	^self onCurrent
		initializeForAppend;
		yourself
]

{ #category : 'instance creation' }
FileSpec class >> appendAndRead [

	^self onCurrent
		initializeForAppendAndRead;
		yourself
]

{ #category : 'instance creation' }
FileSpec class >> appendAndReadBinary [

	^self onCurrent
		initializeForAppendAndReadBinary;
		yourself
]

{ #category : 'instance creation' }
FileSpec class >> appendBinary [

	^self onCurrent
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
FileSpec class >> onClient [
	^self on: FileSystem clientDisk
]

{ #category : 'instance creation' }
FileSpec class >> onCurrent [
	^self new
]

{ #category : 'instance creation' }
FileSpec class >> read [
	^self onCurrent
		initializeForRead;
		yourself
]

{ #category : 'instance creation' }
FileSpec class >> readBinary [

	^self onCurrent
		initializeForReadBinary;
		yourself
]

{ #category : 'instance creation' }
FileSpec class >> readWrite [

	^self onCurrent
		initializeForReadWrite;
		yourself
]

{ #category : 'instance creation' }
FileSpec class >> readWriteBinary [

	^self onCurrent
		initializeForReadWriteBinary;
		yourself
]

{ #category : 'instance creation' }
FileSpec class >> readWriteTruncated [

	^self onCurrent
		initializeForReadWriteTruncated;
		yourself
]

{ #category : 'instance creation' }
FileSpec class >> readWriteTruncatedBinary [

	^self onCurrent
		initializeForReadWriteTruncatedBinary;
		yourself
]

{ #category : 'instance creation' }
FileSpec class >> write [

	^self onCurrent
		initializeForWrite;
		yourself
]

{ #category : 'instance creation' }
FileSpec class >> writeBinary [

	^self onCurrent
		initializeForWriteBinary;
		yourself
]

{ #category : 'instance creation' }
FileSpec class >> writeHighZipped [

	^self onCurrent
		initializeForWriteHighZipped;
		yourself
]

{ #category : 'instance creation' }
FileSpec class >> writeLowZipped [

	^self onCurrent
		initializeForWriteLowZipped;
		yourself
]

{ #category : 'instance creation' }
FileSpec class >> writeZipped [

	^self onCurrent
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
	self fileOptions: FileOptions appendAndRead.
	self beUtf8Encoded
]

{ #category : 'initialization' }
FileSpec >> initializeForAppendAndReadBinary [
	self fileOptions: FileOptions appendAndReadBinary.
	self beUtf8Encoded
]

{ #category : 'initialization' }
FileSpec >> initializeForAppendBinary [
	self fileOptions: FileOptions appendBinary.
	self beUtf8Encoded
]

{ #category : 'initialization' }
FileSpec >> initializeForRead [
	self fileOptions: FileOptions read.
	self beUtf8Encoded
]

{ #category : 'initialization' }
FileSpec >> initializeForReadBinary [
	self fileOptions: FileOptions readBinary.
	self beUtf8Encoded
]

{ #category : 'initialization' }
FileSpec >> initializeForReadWrite [
	self fileOptions: FileOptions readWrite.
	self beUtf8Encoded
]

{ #category : 'initialization' }
FileSpec >> initializeForReadWriteBinary [
	self fileOptions: FileOptions readWriteBinary.
	self beUtf8Encoded
]

{ #category : 'initialization' }
FileSpec >> initializeForReadWriteTruncated [
	self fileOptions: FileOptions readWriteTruncated.
	self beUtf8Encoded
]

{ #category : 'initialization' }
FileSpec >> initializeForReadWriteTruncatedBinary [
	self fileOptions: FileOptions readWriteTruncatedBinary.
	self beUtf8Encoded
]

{ #category : 'initialization' }
FileSpec >> initializeForWrite [
	self fileOptions: FileOptions write.
	self beUtf8Encoded
]

{ #category : 'initialization' }
FileSpec >> initializeForWriteBinary [
	self fileOptions: FileOptions writeBinary.
	self beUtf8Encoded
]

{ #category : 'initialization' }
FileSpec >> initializeForWriteHighZipped [
	self fileOptions: FileOptions writeWithHighCompression
]

{ #category : 'initialization' }
FileSpec >> initializeForWriteLowZipped [
	self fileOptions: FileOptions writeWithLowCompression
]

{ #category : 'initialization' }
FileSpec >> initializeForWriteZipped [
	self fileOptions: FileOptions write.
	self fileOptions fileType: FileGzipTypeFlag none
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
FileSpec >> modeString [
	^self fileOptions modeString
]

{ #category : 'accessing' }
FileSpec >> store [
	^store
]

{ #category : 'accessing' }
FileSpec >> store: anObject [
	store := anObject
]
