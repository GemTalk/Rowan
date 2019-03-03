Class {
	#name : 'StreamSpec',
	#superclass : 'Object',
	#instVars : [
		'fileOptions',
		'encoding',
		'store'
	],
	#category : 'FileSystem-Core'
}

{ #category : 'instance creation' }
StreamSpec class >> append [

	^self new
		initializeForAppend;
		yourself
]

{ #category : 'instance creation' }
StreamSpec class >> new [
	^self on: FileSystem disk
]

{ #category : 'instance creation' }
StreamSpec class >> on: aFileSystem [
	"Default is to be on the local disk filesystem."
	^self basicNew
		initializeOn: aFileSystem;
		yourself
]

{ #category : 'instance creation' }
StreamSpec class >> read [
	^self new
		initializeForRead;
		yourself
]

{ #category : 'instance creation' }
StreamSpec class >> readWrite [

	^self new
		initializeForReadWrite;
		yourself
]

{ #category : 'instance creation' }
StreamSpec class >> write [

	^self new
		initializeForWrite;
		yourself
]

{ #category : 'convenience - file options' }
StreamSpec >> append [
	self fileOptions append
]

{ #category : 'convenience - encoding' }
StreamSpec >> asciiEncoded [
	self encoding: 'ascii'
]

{ #category : 'convenience' }
StreamSpec >> beUnixStore [
	self store: UnixStore createDefault
]

{ #category : 'convenience' }
StreamSpec >> beWindowsStore [
	self store: WindowsStore createDefault
]

{ #category : 'stream creation' }
StreamSpec >> binaryFileStreamOn: aFileReference [

	^(aFileReference fileSystem open: aFileReference options: self fileOptions) binaryStream
]

{ #category : 'convenience - file type' }
StreamSpec >> binaryFileType [
	self fileOptions binaryFileType
]

{ #category : 'convenience' }
StreamSpec >> bufferedStreamClass [
	^self isWritable 
		ifTrue: [ZnBufferedWriteStream] 
		ifFalse: [ZnBufferedReadStream]
]

{ #category : 'stream creation' }
StreamSpec >> bufferedStreamFor: aFileReference [

	^ self bufferedStreamClass on: (self binaryFileStreamOn: aFileReference)
]

{ #category : 'convenience' }
StreamSpec >> characterStreamClass [
	^self isWritable 
		ifTrue: [ZnCharacterWriteStream] 
		ifFalse: [ZnCharacterReadStream]
]

{ #category : 'convenience - file options' }
StreamSpec >> create [
	self fileOptions create
]

{ #category : 'stream creation' }
StreamSpec >> encodedStreamFor: aFileReference [

	^ self characterStreamClass
		on: (self bufferedStreamClass on: (self binaryFileStreamOn: aFileReference))
		encoding: self encoding
]

{ #category : 'accessing' }
StreamSpec >> encoding [
	^encoding
]

{ #category : 'accessing' }
StreamSpec >> encoding: anObject [
	encoding := anObject
]

{ #category : 'convenience - file options' }
StreamSpec >> exclusive [
	self fileOptions exclusive
]

{ #category : 'accessing' }
StreamSpec >> fileOptions [
	^fileOptions
]

{ #category : 'accessing' }
StreamSpec >> fileOptions: aFileOptions [
	aFileOptions parent: self.
	fileOptions := aFileOptions
]

{ #category : 'convenience' }
StreamSpec >> fileType [
	^self fileOptions fileType
]

{ #category : 'convenience - file type' }
StreamSpec >> gzipHighCompression [
	self fileOptions gzipHighCompression
]

{ #category : 'convenience - file type' }
StreamSpec >> gzipLowCompression [
	self fileOptions gzipLowCompression
]

{ #category : 'initialization' }
StreamSpec >> initialize [
	self utf8Encoded.
	self fileOptions: FileOptions new
]

{ #category : 'initialization' }
StreamSpec >> initializeForAppend [
	self fileOptions write create append
]

{ #category : 'initialization' }
StreamSpec >> initializeForRead [
	self fileOptions read
]

{ #category : 'initialization' }
StreamSpec >> initializeForReadWrite [
	self fileOptions readWrite
]

{ #category : 'initialization' }
StreamSpec >> initializeForWrite [
	self fileOptions write create truncate
]

{ #category : 'initialization' }
StreamSpec >> initializeOn: aFileSystem [
	self initialize.
	self store: aFileSystem store
]

{ #category : 'testing' }
StreamSpec >> isClientFileSystem [

	^false
]

{ #category : 'testing' }
StreamSpec >> isGzipped [

	^self fileOptions isGzipped
]

{ #category : 'convenience - encoding' }
StreamSpec >> iso88591Encoded [
	self encoding: 'iso88591'
]

{ #category : 'testing' }
StreamSpec >> isReadable [

	^self fileOptions isReadable
]

{ #category : 'testing' }
StreamSpec >> isWritable [

	^self fileOptions isWritable
]

{ #category : 'convenience - encoding' }
StreamSpec >> latin1Encoded [
	self encoding: 'latin1'
]

{ #category : 'printing' }
StreamSpec >> mode [
	^self fileOptions mode
]

{ #category : 'convenience - encoding' }
StreamSpec >> nullEncoded [
	self encoding: 'null'
]

{ #category : 'convenience - file options' }
StreamSpec >> read [
	self fileOptions read
]

{ #category : 'convenience - file options' }
StreamSpec >> readWrite [
	self fileOptions readWrite
]

{ #category : 'accessing' }
StreamSpec >> store [
	^store
]

{ #category : 'accessing' }
StreamSpec >> store: anObject [
	store := anObject
]

{ #category : 'convenience - file type' }
StreamSpec >> textFileType [
	self fileOptions textFileType
]

{ #category : 'convenience - file options' }
StreamSpec >> truncate [
	self fileOptions truncate
]

{ #category : 'convenience - encoding' }
StreamSpec >> utf16Encoded [
	self encoding: 'utf16'
]

{ #category : 'convenience - encoding' }
StreamSpec >> utf32Encoded [
	self encoding: 'utf32'
]

{ #category : 'convenience - encoding' }
StreamSpec >> utf8Encoded [
	self encoding: 'utf8'
]

{ #category : 'convenience - file options' }
StreamSpec >> write [
	self fileOptions write
]
