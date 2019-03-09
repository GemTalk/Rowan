Class {
	#name : 'AbstractStreamSpec',
	#superclass : 'Object',
	#instVars : [
		'fileOptions',
		'store'
	],
	#category : 'FileSystem-Core'
}

{ #category : 'instance creation' }
AbstractStreamSpec class >> new [
	^self basicNew
		initialize;
		store: (FileSystem disk) store;
		yourself
]

{ #category : 'instance creation' }
AbstractStreamSpec class >> newAppend [

	^self new
		initializeForAppend;
		yourself
]

{ #category : 'instance creation' }
AbstractStreamSpec class >> newRead [
	^self new
		initializeForRead;
		yourself
]

{ #category : 'instance creation' }
AbstractStreamSpec class >> newReadWrite [

	^self new
		initializeForReadWrite;
		yourself
]

{ #category : 'instance creation' }
AbstractStreamSpec class >> newWrite [

	^self new
		initializeForWrite;
		yourself
]

{ #category : 'instance creation' }
AbstractStreamSpec class >> on: aFileReference options: aFileOptions [
	"Note: This returns an instance of the stream on the fileReference"
	^self basicNew
		initalizeOn: aFileReference options: aFileOptions
		yourself
]

{ #category : 'instance creation' }
AbstractStreamSpec class >> onOptions: aFileOptions [
	^self basicNew
		initalizeOnOptions: aFileOptions
		yourself
]

{ #category : 'private' }
AbstractStreamSpec >> alignStoreWith: aFileReference [
	"Stream Spec is built with the local filesystem. If a foreign filesystem
     was used to build the reference, switch to the foreign filesystem"

	aFileReference fileSystem isForeignFileSystem 
		ifTrue: [self store: aFileReference fileSystem store]
]

{ #category : 'file options configuration' }
AbstractStreamSpec >> append [
	self fileOptions append
]

{ #category : 'private' }
AbstractStreamSpec >> beUnixStore [
	self store: UnixStore createDefault
]

{ #category : 'private' }
AbstractStreamSpec >> beWindowsStore [
	self store: WindowsStore createDefault
]

{ #category : 'file type configuration' }
AbstractStreamSpec >> binaryFileType [
	self fileOptions binaryFileType
]

{ #category : 'private' }
AbstractStreamSpec >> containedStreamOn: aFileReference [
	^(self containedStreamSpec onOptions: self fileOptions) on: aFileReference
]

{ #category : 'configuration' }
AbstractStreamSpec >> containedStreamSpec [
	self subclassResponsibility
]

{ #category : 'file options configuration' }
AbstractStreamSpec >> create [
	self fileOptions create
]

{ #category : 'file options configuration' }
AbstractStreamSpec >> exclusive [
	self fileOptions exclusive
]

{ #category : 'accessing' }
AbstractStreamSpec >> fileOptions [
	^fileOptions
]

{ #category : 'accessing' }
AbstractStreamSpec >> fileOptions: aFileOptions [
	aFileOptions parent: self.
	fileOptions := aFileOptions
]

{ #category : 'file type configuration' }
AbstractStreamSpec >> fileType [
	^self fileOptions fileType
]

{ #category : 'file type configuration' }
AbstractStreamSpec >> gzipHighCompression [
	self fileOptions gzipHighCompression
]

{ #category : 'file type configuration' }
AbstractStreamSpec >> gzipLowCompression [
	self fileOptions gzipLowCompression
]

{ #category : 'initialization' }
AbstractStreamSpec >> initalizeOn: aFileReference options: aFileOptions [
	self initialize.
	self fileOptions: aFileOptions.
	^self on: aFileReference
]

{ #category : 'initialization' }
AbstractStreamSpec >> initalizeOnOptions: aFileOptions [
	self initialize.
	self fileOptions: aFileOptions copy.
	self store: (
		aFileOptions parent 
			ifNil: [FileSystem disk] 
			ifNotNil: [:streamSpec | streamSpec store])
]

{ #category : 'initialization' }
AbstractStreamSpec >> initialize [
	self fileOptions: FileOptions new
]

{ #category : 'initialization' }
AbstractStreamSpec >> initializeForAppend [
	self fileOptions write create append
]

{ #category : 'initialization' }
AbstractStreamSpec >> initializeForRead [
	self fileOptions read
]

{ #category : 'initialization' }
AbstractStreamSpec >> initializeForReadWrite [
	self fileOptions readWrite
]

{ #category : 'initialization' }
AbstractStreamSpec >> initializeForWrite [
	self fileOptions write create truncate
]

{ #category : 'testing' }
AbstractStreamSpec >> isGzipped [

	^self fileOptions isGzipped
]

{ #category : 'testing' }
AbstractStreamSpec >> isReadable [

	^self fileOptions isReadable
]

{ #category : 'testing' }
AbstractStreamSpec >> isReadWritable [

	^self fileOptions isReadWritable
]

{ #category : 'testing' }
AbstractStreamSpec >> isWritable [

	^self fileOptions isWritable
]

{ #category : 'printing' }
AbstractStreamSpec >> mode [
	^self fileOptions mode
]

{ #category : 'streams' }
AbstractStreamSpec >> on: aFileReference [
	self alignStoreWith: aFileReference.
	^ self streamClass on: (self containedStreamOn: aFileReference)
]

{ #category : 'streams' }
AbstractStreamSpec >> on: aFileReference do: aBlock [
	| aStream |
	aStream := self on: aFileReference.
	^ [ aBlock value: aStream ]
			ensure: [ aStream close ]
]

{ #category : 'streams' }
AbstractStreamSpec >> on: aFileReference do: aBlock ifAbsent: anAbsentBlock [
	^aFileReference isFile
		ifTrue: [self on: aFileReference do: aBlock]
		ifFalse: [anAbsentBlock value]
]

{ #category : 'streams' }
AbstractStreamSpec >> on: aFileReference do: aBlock ifPresent: anAbsentBlock [
	^aFileReference isFile
		ifTrue: [anAbsentBlock value]
		ifFalse: [ self on: aFileReference do: aBlock]
]

{ #category : 'file options configuration' }
AbstractStreamSpec >> read [
	self fileOptions read
]

{ #category : 'configuration' }
AbstractStreamSpec >> readStreamClass [
	self subclassResponsibility
]

{ #category : 'file options configuration' }
AbstractStreamSpec >> readWrite [
	self fileOptions readWrite
]

{ #category : 'configuration' }
AbstractStreamSpec >> readWriteStreamClass [
	self subclassResponsibility
]

{ #category : 'accessing' }
AbstractStreamSpec >> store [
	^store ifNil: [^FileSystem disk store]
]

{ #category : 'accessing' }
AbstractStreamSpec >> store: anObject [
	store := anObject
]

{ #category : 'configuration' }
AbstractStreamSpec >> streamClass [
	self isReadWritable ifTrue: [^self readWriteStreamClass].
	self isWritable ifTrue: [^self writeStreamClass] .
	^self readStreamClass
]

{ #category : 'file type configuration' }
AbstractStreamSpec >> textFileType [
	self fileOptions textFileType
]

{ #category : 'file options configuration' }
AbstractStreamSpec >> truncate [
	self fileOptions truncate
]

{ #category : 'file options configuration' }
AbstractStreamSpec >> write [
	self fileOptions write
]

{ #category : 'configuration' }
AbstractStreamSpec >> writeStreamClass [
	self subclassResponsibility
]
