"
This class contains options for a file. The options available are based on the POSIX standards.

This gives a more flexible and nuanced way of defining how a file is to be opened and used. This is following the POSIX file open options. Much of this
information has been garnered from the VA Smalltalk POSIX implmenetion of CfsFielDescriptior and CfsDirectoryDescriptor.

Instance Variables:
mode 			-	This contains the access mode of the file - O_RDONLY | O_RDWR | O_WRONLY. 
						These modes are represented by the classes FileOpenReadOnlyMode, FileOpenReadWriteMode, and FileOpenWriteOnlyMode.
options			- 	This contains a collection of the options which are used to further define how the file is opened, created if missing, truncate, append, etc.
						Options contais instances of subclasses of FileCreateOption

API:
modeString	-	Returns the traditional unix mode string. (See table below.) This is implemented to support GemStone's GsFile file class.
isWritable		-	Returns true/false based on the access mode - O_RDONLY | O_RDWR | O_WRONLY. This is implemen to support Pharo's File file class.



This table shows the mapping between the file options and the traditional unix string for defining file open mode.

Mode                 File Options (Flags)
------				-----------------------------------------
	r     				O_RDONLY
	w            		O_WRONLY | O_CREAT | O_TRUNC
	a    				O_WRONLY | O_CREAT | O_APPEND
	r+            		O_RDWR
	w+				O_RDWR | O_CREAT | O_TRUNC
	a+				O_RDWR | O_CREAT | O_APPEND

The
"
Class {
	#name : 'FileOptions',
	#superclass : 'Object',
	#instVars : [
		'mode',
		'options',
		'fileType',
		'share',
		'permissionOptions',
		'parent'
	],
	#category : 'FileSystem-Disk-Options'
}

{ #category : 'instance creation' }
FileOptions class >> append [
	^self basicNew 
		initializeAppend;
		yourself
]

{ #category : 'instance creation' }
FileOptions class >> appendAndRead [
	^self basicNew
		initializeAppendAndRead;
		yourself
]

{ #category : 'instance creation' }
FileOptions class >> appendAndReadBinary [
	^self appendAndRead
		beBinaryFileType;
		yourself
]

{ #category : 'instance creation' }
FileOptions class >> appendBinary [
	^self append
		beBinaryFileType;
		yourself
]

{ #category : 'instance creation' }
FileOptions class >> new [
	^self basicNew
		initialize;
		yourself
]

{ #category : 'instance creation' }
FileOptions class >> read [
	^self basicNew
		initializeRead;
		yourself
]

{ #category : 'instance creation' }
FileOptions class >> readBinary [
	^self read
		beBinaryFileType;
		yourself
]

{ #category : 'instance creation' }
FileOptions class >> readWrite [
	^self basicNew
		initializeReadWrite;
		yourself
]

{ #category : 'instance creation' }
FileOptions class >> readWriteBinary [
	^self readWrite
		beBinaryFileType;
		yourself
]

{ #category : 'instance creation' }
FileOptions class >> readWriteTruncated [
	^self basicNew
		initializeReadWriteTruncated;
		yourself
]

{ #category : 'instance creation' }
FileOptions class >> readWriteTruncatedBinary [
	^self readWriteTruncated
		beBinaryFileType;
		yourself
]

{ #category : 'instance creation' }
FileOptions class >> write [
	^self basicNew
		initializeWrite;
		yourself
]

{ #category : 'instance creation' }
FileOptions class >> writeBinary [
	^self write
		beBinaryFileType;
		yourself
]

{ #category : 'options' }
FileOptions >> addAppendOption [

	self addOption: FileAppendOption new
]

{ #category : 'options' }
FileOptions >> addCreateOption [

	self addOption: FileCreateOption new
]

{ #category : 'options' }
FileOptions >> addOption: anOption [
	anOption parent: self.
	self options add: anOption
]

{ #category : 'options' }
FileOptions >> addTruncateOption [

	self addOption: FileTruncateOption new
]

{ #category : 'file type' }
FileOptions >> beBinaryFileType [
	self fileType: FileBinaryTypeOption new
]

{ #category : 'sharing' }
FileOptions >> bePrivate [
	self share: FileDenyReadWriteOption new
]

{ #category : 'open mode' }
FileOptions >> beReadMode [
	self mode: FileOpenReadOnlyMode new.
]

{ #category : 'sharing' }
FileOptions >> beReadOnlyShared [
	self share: FileDenyWriteOption new
]

{ #category : 'open mode' }
FileOptions >> beReadWriteMode [
	self mode: FileOpenReadWriteMode new.
]

{ #category : 'sharing' }
FileOptions >> beShared [
	self share: FileDenyNoneOption new
]

{ #category : 'file type' }
FileOptions >> beTextFileType [
	self fileType: FileTextTypeOption new
]

{ #category : 'open mode' }
FileOptions >> beWriteMode [
	self mode: FileOpenWriteOnlyMode new.
]

{ #category : 'sharing' }
FileOptions >> beWriteOnlyShared [
	self share: FileDenyReadOption new
]

{ #category : 'convenience' }
FileOptions >> compressionPolicy [
	^self parent ifNotNil: [:fs | fs compressionPolicy]
]

{ #category : 'options' }
FileOptions >> fileOpenAndOptionsValue [
	^self fileOpenModeValue + self fileOptionsValue
]

{ #category : 'options' }
FileOptions >> fileOpenModeValue [
	^self mode posixValue
]

{ #category : 'options' }
FileOptions >> fileOptionsValue [
	^self options inject: 0 into: [:total :ea | total + ea posixValue]
]

{ #category : 'accessing' }
FileOptions >> fileType [
	^fileType
]

{ #category : 'accessing' }
FileOptions >> fileType: aFileTypeOption [
	aFileTypeOption parent: self.
	fileType := aFileTypeOption
]

{ #category : 'testing' }
FileOptions >> hasFileAppendOption [
	^self hasOption: FileAppendOption
]

{ #category : 'testing' }
FileOptions >> hasFileCreateOption [
	^self hasOption: FileCreateOption
]

{ #category : 'testing' }
FileOptions >> hasFileTruncateOption [
	^self hasOption: FileTruncateOption
]

{ #category : 'testing' }
FileOptions >> hasOption: anOptionClass [
	^(self options detect: [:ea | ea isKindOf: anOptionClass] ifNone: [nil]) notNil
]

{ #category : 'initialization' }
FileOptions >> initialize [
	self options: OrderedCollection new.
	self permissionOptions: OrderedCollection new.
	self beShared
]

{ #category : 'initialization' }
FileOptions >> initializeAppend [
	self initialize.
	self beWriteMode.
	self addCreateOption.
	self addAppendOption
]

{ #category : 'initialization' }
FileOptions >> initializeAppendAndRead [
	self initialize.
	self beReadWriteMode.
	self addCreateOption.
	self addAppendOption
]

{ #category : 'initialization' }
FileOptions >> initializeRead [
	self initialize.
	self beReadMode
]

{ #category : 'initialization' }
FileOptions >> initializeReadWrite [
	self initialize.
	self beReadWriteMode
]

{ #category : 'initialization' }
FileOptions >> initializeReadWriteTruncated [
	self initialize.
	self initializeReadWrite.
	self addCreateOption.
	self addTruncateOption
]

{ #category : 'initialization' }
FileOptions >> initializeWrite [
	self initialize.
	self beWriteMode.
	self addCreateOption.
	self addTruncateOption
]

{ #category : 'testing' }
FileOptions >> isClientFileSystem [
	"This needs more work"
	^false
]

{ #category : 'testing' }
FileOptions >> isGzipped [
	^self parent isGzipped
]

{ #category : 'testing' }
FileOptions >> isReadable [
	^self mode isReadable
]

{ #category : 'testing' }
FileOptions >> isWritable [
	^self mode isWritable
]

{ #category : 'accessing' }
FileOptions >> mode [
	^mode
]

{ #category : 'accessing' }
FileOptions >> mode: aFileOpenMode [
	aFileOpenMode parent: self.
	mode :=  aFileOpenMode
]

{ #category : 'printing' }
FileOptions >> modeString [
	| aStream |
	aStream := WriteStream on: String new.
	self printOpenModeStringOn: aStream.
	self compressionPolicy ifNotNil: [:cp | cp printOpenModeStringOn: aStream].
	^aStream contents
]

{ #category : 'accessing' }
FileOptions >> options [
	^options
]

{ #category : 'accessing' }
FileOptions >> options: aCollection [
	options := aCollection
]

{ #category : 'accessing' }
FileOptions >> parent [
	^parent
]

{ #category : 'accessing' }
FileOptions >> parent: anObject [
	parent := anObject
]

{ #category : 'accessing' }
FileOptions >> permissionOptions [
	^permissionOptions
]

{ #category : 'accessing' }
FileOptions >> permissionOptions: aCollection [
	permissionOptions := aCollection
]

{ #category : 'printing' }
FileOptions >> printOpenModeStringOn: aStream [

	self mode printOpenModeStringOn: aStream.
	self fileType ifNotNil: [:ft | ft printOpenModeStringOn: aStream.]
]

{ #category : 'options' }
FileOptions >> removeOption: anOption [
	anOption parent: nil.
	self options remove: anOption ifAbsent: []
]

{ #category : 'accessing' }
FileOptions >> share [
	^share
]

{ #category : 'accessing' }
FileOptions >> share: aFileShareOption [
	aFileShareOption parent: self.
	share := aFileShareOption
]
