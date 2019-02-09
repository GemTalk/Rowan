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
FileOptions >> accessPermissions [

	self permissionOptions isEmpty ifTrue: [^FileSystemPermission default posixPermission].
	^self permissionOptions inject: 0 into: [:total :ea | total + ea posixValue]
]

{ #category : 'convenience' }
FileOptions >> accessRule [
	^self mode accessRule
]

{ #category : 'options - adding and removing' }
FileOptions >> addAppendFlag [

	self addOption: FileAppendFlag new
]

{ #category : 'options - adding and removing' }
FileOptions >> addCreateFlag [

	self addOption: FileCreateFlag new
]

{ #category : 'options - adding and removing' }
FileOptions >> addExclusiveFlag [

	self addOption: FileExclusiveFlag new
]

{ #category : 'options - adding and removing' }
FileOptions >> addOption: anOption [
	anOption parent: self.
	self options add: anOption
]

{ #category : 'options - adding and removing' }
FileOptions >> addTruncateFlag [

	self addOption: FileTruncateFlag new
]

{ #category : 'options' }
FileOptions >> appendFlag [
	^self optionForClass: FileAppendFlag
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
	self mode: FileOpenReadOnlyFlag new.
]

{ #category : 'sharing' }
FileOptions >> beReadOnlyShared [
	self share: FileDenyWriteFlag new
]

{ #category : 'open mode' }
FileOptions >> beReadWriteMode [
	self mode: FileOpenReadWriteFlag new.
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
	self mode: FileOpenWriteOnlyFlag new.
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

{ #category : 'options' }
FileOptions >> fileCreateFlag [
	^self optionForClass: FileCreateFlag
]

{ #category : 'options' }
FileOptions >> fileExclusiveFlag [
	^self optionForClass: FileExclusiveFlag
]

{ #category : 'options' }
FileOptions >> fileOpenAndOptionsValue [
	^self fileOpenModeValue + self fileOpenOptionsValue
]

{ #category : 'options' }
FileOptions >> fileOpenModeValue [
	^self mode optionValue
]

{ #category : 'options' }
FileOptions >> fileOpenOptionsValue [
	^self options inject: 0 into: [:total :ea | total + ea optionValue]
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
FileOptions >> hasExclsiveFlag [
	^self hasOption: FileExclusiveFlag
]

{ #category : 'testing' }
FileOptions >> hasFileAppendFlag [
	^self hasOption: FileAppendFlag
]

{ #category : 'testing' }
FileOptions >> hasFileCreateFlag [
	^self hasOption: FileCreateFlag
]

{ #category : 'testing' }
FileOptions >> hasFileTruncateFlag [
	^self hasOption: FileTruncateFlag
]

{ #category : 'testing' }
FileOptions >> hasOption: anPosixFlagClass [
	^(self optionForClass: anPosixFlagClass) notNil
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
	^aStream contents
]

{ #category : 'options' }
FileOptions >> optionForClass: anPosixFlagClass [
	^self options detect: [:ea | ea isKindOf: anPosixFlagClass] ifNone: [nil]
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

{ #category : 'options - adding and removing' }
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

{ #category : 'convenience' }
FileOptions >> shareRule [
	^self share shareRule
]

{ #category : 'convenience' }
FileOptions >> store [
	^self parent ifNil: [FileSystem disk store] ifNotNil: [:fs | fs store]
]

{ #category : 'options' }
FileOptions >> truncateFlag [
	^self optionForClass: FileTruncateFlag
]

{ #category : 'convenience' }
FileOptions >> truncateRule [
	^self truncateFlag
		ifNil: [self store class noTruncateRule] 
		ifNotNil: [:option | option truncateRule]
]
