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
		'openPosixFlag',
		'posixFlags',
		'fileType',
		'share',
		'permissionFlags',
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
FileOptions class >> appendWithHighCompression [
	^self basicNew
		initializeAppendWithHighCompression;
		yourself
]

{ #category : 'instance creation' }
FileOptions class >> appendWithLowCompression [
	^self basicNew
		initializeAppendWithLowCompression;
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

{ #category : 'instance creation' }
FileOptions class >> writeWithHighCompression [
	^self write
		initializeWriteWithHighCompression;
		yourself
]

{ #category : 'instance creation' }
FileOptions class >> writeWithLowCompression [
	^self write
		initializeWriteWithLowCompression;
		yourself
]

{ #category : 'permissions' }
FileOptions >> accessPermissionValue [

	self permissionFlags isEmpty ifTrue: [^FileSystemPermission default posixPermission].
	^self permissionFlags inject: 0 into: [:total :ea | total bitOr: ea posixValue]
]

{ #category : 'convenience' }
FileOptions >> accessRule [
	^self openPosixFlag accessRule
]

{ #category : 'posix flags - adding and removing' }
FileOptions >> addAppendFlag [

	self addFlag: FileAppendFlag new
]

{ #category : 'posix flags - adding and removing' }
FileOptions >> addCreateFlag [

	self addFlag: FileCreateFlag new
]

{ #category : 'posix flags - adding and removing' }
FileOptions >> addExclusiveFlag [

	self addFlag: FileExclusiveFlag new
]

{ #category : 'posix flags - adding and removing' }
FileOptions >> addFlag: aPosixFag [
	aPosixFag parent: self.
	self posixFlags add: aPosixFag
]

{ #category : 'posix flags - adding and removing' }
FileOptions >> addTruncateFlag [

	self addFlag: FileTruncateFlag new
]

{ #category : 'posix flags' }
FileOptions >> appendFlag [
	^self posixFlagForClass: FileAppendFlag
]

{ #category : 'file type' }
FileOptions >> beBinaryFileType [
	self fileType: FileBinaryTypeFlag new
]

{ #category : 'file type' }
FileOptions >> beHighCompression [
	self fileType: FileGzipTypeFlag high
]

{ #category : 'file type' }
FileOptions >> beLowCompression [
	self fileType: FileGzipTypeFlag low
]

{ #category : 'sharing' }
FileOptions >> bePrivate [
	self share: FileDenyReadWriteFlag new
]

{ #category : 'open mode' }
FileOptions >> beReadMode [
	self openPosixFlag: FileOpenReadOnlyFlag new.
]

{ #category : 'sharing' }
FileOptions >> beReadOnlyShared [
	self share: FileDenyWriteFlag new
]

{ #category : 'open mode' }
FileOptions >> beReadWriteMode [
	self openPosixFlag: FileOpenReadWriteFlag new.
]

{ #category : 'sharing' }
FileOptions >> beShared [
	self share: FileDenyNoneFlag new
]

{ #category : 'file type' }
FileOptions >> beTextFileType [
	self fileType: FileTextTypeFlag new
]

{ #category : 'open mode' }
FileOptions >> beWriteMode [
	self openPosixFlag: FileOpenWriteOnlyFlag new.
]

{ #category : 'sharing' }
FileOptions >> beWriteOnlyShared [
	self share: FileDenyReadFlag new
]

{ #category : 'convenience' }
FileOptions >> createRule [
	^self fileCreateFlag 
		ifNil: [self store class openExistingRule] 
		ifNotNil: [:option | option createRule]
]

{ #category : 'posix flags' }
FileOptions >> fileCreateFlag [
	^self posixFlagForClass: FileCreateFlag
]

{ #category : 'posix flags' }
FileOptions >> fileExclusiveFlag [
	^self posixFlagForClass: FileExclusiveFlag
]

{ #category : 'posix flags' }
FileOptions >> fileOpenPosixFlagValue [
	^self openPosixFlag flagValue
]

{ #category : 'posix flags' }
FileOptions >> fileOpenValue [
	^self fileOpenPosixFlagValue bitOr: self otherFileOpenPosixFlagsValue
]

{ #category : 'accessing' }
FileOptions >> fileType [
	^fileType
]

{ #category : 'accessing' }
FileOptions >> fileType: aFileTypeFlag [
	aFileTypeFlag parent: self.
	fileType := aFileTypeFlag
]

{ #category : 'testing' }
FileOptions >> hasExclsiveFlag [
	^self hasPosixFlag: FileExclusiveFlag
]

{ #category : 'testing' }
FileOptions >> hasFileAppendFlag [
	^self hasPosixFlag: FileAppendFlag
]

{ #category : 'testing' }
FileOptions >> hasFileCreateFlag [
	^self hasPosixFlag: FileCreateFlag
]

{ #category : 'testing' }
FileOptions >> hasFileTruncateFlag [
	^self hasPosixFlag: FileTruncateFlag
]

{ #category : 'testing' }
FileOptions >> hasPosixFlag: anPosixFlagClass [
	^(self posixFlagForClass: anPosixFlagClass) notNil
]

{ #category : 'initialization' }
FileOptions >> initialize [
	self posixFlags: OrderedCollection new.
	self permissionFlags: OrderedCollection new.
	self beShared
]

{ #category : 'initialization' }
FileOptions >> initializeAppend [
	self initialize.
	self beWriteMode.
	self addCreateFlag.
	self addAppendFlag
]

{ #category : 'initialization' }
FileOptions >> initializeAppendAndRead [
	self initialize.
	self beReadWriteMode.
	self addCreateFlag.
	self addAppendFlag
]

{ #category : 'initialization' }
FileOptions >> initializeAppendWithHighCompression [
	self initializeAppend.
	self beHighCompression
]

{ #category : 'initialization' }
FileOptions >> initializeAppendWithLowCompression [
	self initializeAppend.
	self beLowCompression
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
	self addCreateFlag.
	self addTruncateFlag
]

{ #category : 'initialization' }
FileOptions >> initializeWrite [
	self initialize.
	self beWriteMode.
	self addCreateFlag.
	self addTruncateFlag
]

{ #category : 'initialization' }
FileOptions >> initializeWriteWithHighCompression [
	self initializeWrite.
	self beHighCompression
]

{ #category : 'initialization' }
FileOptions >> initializeWriteWithLowCompression [
	self initializeWrite.
	self beLowCompression
]

{ #category : 'testing' }
FileOptions >> isForeignFileSystem [
	^self store isForeignFileSystem
]

{ #category : 'testing' }
FileOptions >> isGzipped [
	^(self fileType  ifNil: [^false]) isGzipped
]

{ #category : 'testing' }
FileOptions >> isReadable [
	^self openPosixFlag isReadable
]

{ #category : 'testing' }
FileOptions >> isWritable [
	^self openPosixFlag isWritable
]

{ #category : 'printing' }
FileOptions >> mode [
	| aStream |
	aStream := WriteStream on: String new.
	self printOpenModeOn: aStream.
	^aStream contents
]

{ #category : 'accessing' }
FileOptions >> openPosixFlag [
	^openPosixFlag
]

{ #category : 'accessing' }
FileOptions >> openPosixFlag: aFileOpenPosixFlag [
	aFileOpenPosixFlag parent: self.
	openPosixFlag :=  aFileOpenPosixFlag
]

{ #category : 'posix flags' }
FileOptions >> otherFileOpenPosixFlagsValue [
	^self posixFlags inject: 0 into: [:total :ea | total bitOr: ea flagValue]
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
FileOptions >> permissionFlags [
	^permissionFlags
]

{ #category : 'accessing' }
FileOptions >> permissionFlags: aCollection [
	permissionFlags := aCollection
]

{ #category : 'posix flags - adding and removing' }
FileOptions >> posixFlagForClass: anPosixFlagClass [
	^self posixFlags detect: [:ea | ea class = anPosixFlagClass] ifNone: []
]

{ #category : 'accessing' }
FileOptions >> posixFlags [
	^posixFlags
]

{ #category : 'accessing' }
FileOptions >> posixFlags: aCollection [
	posixFlags := aCollection
]

{ #category : 'printing' }
FileOptions >> printOpenModeOn: aStream [

	self openPosixFlag printOpenModeOn: aStream.
	self fileType ifNotNil: [:ft | ft printOpenModeOn: aStream.]
]

{ #category : 'posix flags - adding and removing' }
FileOptions >> removeFlag: aPosixFlag [
	aPosixFlag parent: nil.
	self posixFlags remove: aPosixFlag ifAbsent: []
]

{ #category : 'accessing' }
FileOptions >> share [
	^share
]

{ #category : 'accessing' }
FileOptions >> share: aFileShareFlag [
	aFileShareFlag parent: self.
	share := aFileShareFlag
]

{ #category : 'convenience' }
FileOptions >> shareRule [
	^self share shareRule
]

{ #category : 'convenience' }
FileOptions >> store [
	^self parent ifNil: [FileSystem disk store] ifNotNil: [:fs | fs store]
]

{ #category : 'posix flags' }
FileOptions >> truncateFlag [
	^self posixFlagForClass: FileTruncateFlag
]

{ #category : 'convenience' }
FileOptions >> truncateRule [
	^self truncateFlag
		ifNil: [self store class noTruncateRule] 
		ifNotNil: [:option | option truncateRule]
]
