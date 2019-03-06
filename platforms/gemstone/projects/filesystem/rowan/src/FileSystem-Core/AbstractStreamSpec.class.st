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
AbstractStreamSpec class >> append [

	^self new
		initializeForAppend;
		yourself
]

{ #category : 'instance creation' }
AbstractStreamSpec class >> new [
	^self basicNew
		initialize;
		store: FileSystem disk;
		yourself
]

{ #category : 'instance creation' }
AbstractStreamSpec class >> onOptions: aFileOptions [
	^self basicNew
		fileOptions: aFileOptions copy;
		store: aFileOptions parent store;
		yourself
]

{ #category : 'instance creation' }
AbstractStreamSpec class >> read [
	^self new
		initializeForRead;
		yourself
]

{ #category : 'instance creation' }
AbstractStreamSpec class >> readWrite [

	^self new
		initializeForReadWrite;
		yourself
]

{ #category : 'instance creation' }
AbstractStreamSpec class >> write [

	^self new
		initializeForWrite;
		yourself
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

{ #category : 'configuration' }
AbstractStreamSpec >> childStreamSpec [
	^BufferedStreamSpec
]

{ #category : 'private' }
AbstractStreamSpec >> containedStreamOn: aFileReference [
	^(self childStreamSpec onOptions: self fileOptions) on: aFileReference
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

{ #category : 'stream creation' }
AbstractStreamSpec >> on: aFileReference [
	^ self streamClass on: (self containedStreamOn: aFileReference)
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
