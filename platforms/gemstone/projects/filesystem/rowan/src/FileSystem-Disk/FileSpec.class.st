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
FileSpec class >> readWrite [

	^self new
		initializeForReadWrite;
		yourself
]

{ #category : 'instance creation' }
FileSpec class >> write [

	^self new
		initializeForWrite;
		yourself
]

{ #category : 'convenience - file options' }
FileSpec >> append [
	self fileOptions append
]

{ #category : 'convenience - encoding' }
FileSpec >> asciiEncoded [
	self encoding: 'ascii'
]

{ #category : 'convenience' }
FileSpec >> beUnixStore [
	self store: UnixStore createDefault
]

{ #category : 'convenience' }
FileSpec >> beWindowsStore [
	self store: WindowsStore createDefault
]

{ #category : 'stream creation' }
FileSpec >> binaryFileStreamOn: aFileReference [

	^(aFileReference fileSystem open: aFileReference options: self fileOptions) binaryStream
]

{ #category : 'convenience - file type' }
FileSpec >> binaryFileType [
	self fileOptions binaryFileType
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

{ #category : 'convenience - file options' }
FileSpec >> create [
	self fileOptions create
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

{ #category : 'convenience - file options' }
FileSpec >> exclusive [
	self fileOptions exclusive
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

{ #category : 'convenience - file type' }
FileSpec >> gzipHighCompression [
	self fileOptions gzipHighCompression
]

{ #category : 'convenience - file type' }
FileSpec >> gzipLowCompression [
	self fileOptions gzipLowCompression
]

{ #category : 'initialization' }
FileSpec >> initialize [
	self utf8Encoded.
	self fileOptions: FileOptions new
]

{ #category : 'initialization' }
FileSpec >> initializeForAppend [
	self fileOptions write create append
]

{ #category : 'initialization' }
FileSpec >> initializeForRead [
	self fileOptions read
]

{ #category : 'initialization' }
FileSpec >> initializeForReadWrite [
	self fileOptions readWrite
]

{ #category : 'initialization' }
FileSpec >> initializeForWrite [
	self fileOptions write create truncate
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

{ #category : 'convenience - encoding' }
FileSpec >> iso88591Encoded [
	self encoding: 'iso88591'
]

{ #category : 'testing' }
FileSpec >> isReadable [

	^self fileOptions isReadable
]

{ #category : 'testing' }
FileSpec >> isWritable [

	^self fileOptions isWritable
]

{ #category : 'convenience - encoding' }
FileSpec >> latin1Encoded [
	self encoding: 'latin1'
]

{ #category : 'printing' }
FileSpec >> mode [
	^self fileOptions mode
]

{ #category : 'convenience - encoding' }
FileSpec >> nullEncoded [
	self encoding: 'null'
]

{ #category : 'convenience - file options' }
FileSpec >> read [
	self fileOptions read
]

{ #category : 'convenience - file options' }
FileSpec >> readWrite [
	self fileOptions readWrite
]

{ #category : 'accessing' }
FileSpec >> store [
	^store
]

{ #category : 'accessing' }
FileSpec >> store: anObject [
	store := anObject
]

{ #category : 'convenience - file type' }
FileSpec >> textFileType [
	self fileOptions textFileType
]

{ #category : 'convenience - file options' }
FileSpec >> truncate [
	self fileOptions truncate
]

{ #category : 'convenience - encoding' }
FileSpec >> utf16Encoded [
	self encoding: 'utf16'
]

{ #category : 'convenience - encoding' }
FileSpec >> utf32Encoded [
	self encoding: 'utf32'
]

{ #category : 'convenience - encoding' }
FileSpec >> utf8Encoded [
	self encoding: 'utf8'
]

{ #category : 'convenience - file options' }
FileSpec >> write [
	self fileOptions write
]
