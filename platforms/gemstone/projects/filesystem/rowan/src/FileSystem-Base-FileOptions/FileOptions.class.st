"
This class contains options for a file. The options available are based on the POSIX standards.

This gives a more flexible and nuanced way of defining how a file is to be opened and used. This is following the POSIX file open options. Much of this
information has been garnered from the VA Smalltalk POSIX implmenetion of CfsFielDescriptior and CfsDirectoryDescriptor.

Instance Variables:
openModeFlag -	This contains the access mode of the file - O_RDONLY | O_RDWR | O_WRONLY.
						These modes are represented by the classes FileOpenReadOnlyMode, FileOpenReadWriteMode, and FileOpenWriteOnlyMode.
posixFlags		- 	This contains a collection of the options which are used to further define how the file is opened, created if missing, truncate, append, etc.
						Options contais instances of subclasses of FileCreateOption
fileType			-   This defines the file type. Available types are Binary, Text, Gzip (Currently GemStone only). The former two are only relevant to Windows.
						The difference between Binary and Text is how cr/lf is handled. In Unicode this distinction is non-sensical so Binary is recommended for Unicode files.
share 			-	Indicates how the file is to be shared with others.

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

See https://linux.die.net/man/3/fopen for more details on the fopen open modes.
"
Class {
	#name : 'FileOptions',
	#superclass : 'Object',
	#instVars : [
		'openModeFlag',
		'mode',
		'posixFlags',
		'fileType',
		'share',
		'permissionFlags',
		'parent',
		'store'
	],
	#category : 'FileSystem-Base-FileOptions'
}

{ #category : 'instance creation' }
FileOptions class >> append [
	^self new
		initializeAppend;
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
	^self new
		initializeRead;
		yourself
]

{ #category : 'instance creation' }
FileOptions class >> readWrite [
	^self new
		initializeReadWrite;
		yourself
]

{ #category : 'instance creation' }
FileOptions class >> write [
	^self new
		initializeWrite;
		yourself
]

{ #category : 'permissions' }
FileOptions >> accessPermissions [

	self permissionFlags isEmpty ifTrue: [^#FileSystemPermission default posixPermission].
	^self permissionFlags inject: 0 into: [:total :ea | total bitOr: ea posixFlagValue]
]

{ #category : 'convenience' }
FileOptions >> accessRule [
	^self openModeFlag accessRule
]

{ #category : 'posix flags - adding and removing' }
FileOptions >> addAppendFlag [
	self removeTruncateFlag.
	self addPosixFlag: FileAppendFlag new
]

{ #category : 'posix flags - adding and removing' }
FileOptions >> addCreateFlag [

	self addPosixFlag: FileCreateFlag new
]

{ #category : 'posix flags - adding and removing' }
FileOptions >> addExclusiveFlag [

	self addPosixFlag: FileExclusiveFlag new
]

{ #category : 'permissions' }
FileOptions >> addPermissionFlag: aPermissionFlag [
	(self hasPermissionsFlag: aPermissionFlag class) ifTrue: [^self].
	aPermissionFlag parent: self.
	self permissionFlags add: aPermissionFlag
]

{ #category : 'posix flags - adding and removing' }
FileOptions >> addPosixFlag: aPosixFlag [
	(self hasPosixFlag: aPosixFlag class) ifTrue: [^self].
	aPosixFlag parent: self.
	self posixFlags add: aPosixFlag
]

{ #category : 'posix flags - adding and removing' }
FileOptions >> addTruncateFlag [
	self removeAppendFlag.
	self addPosixFlag: FileTruncateFlag new
]

{ #category : 'initialization' }
FileOptions >> alignStoreWith: anObject [
	"FileOptions is built with the local filesystem. If a foreign filesystem
     was used to build the reference, switch to the foreign filesystem"

	anObject store isForeignFileSystem 
		ifTrue: [self store: anObject store]
]

{ #category : 'commands' }
FileOptions >> append [
	self hasNoReadOrWriteMode ifTrue: [^self signalOpenModeFlagNotDefined].
	self addAppendFlag
]

{ #category : 'posix flags' }
FileOptions >> appendFlag [
	^self posixFlagForClass: FileAppendFlag
]

{ #category : 'sharing' }
FileOptions >> beNonShared [
	self share: FileDenyReadWriteFlag new
]

{ #category : 'open mode flag' }
FileOptions >> beReadMode [
	self openModeFlag: FileOpenReadOnlyFlag new.
]

{ #category : 'sharing' }
FileOptions >> beReadOnlyShared [
	self share: FileDenyWriteFlag new
]

{ #category : 'open mode flag' }
FileOptions >> beReadWriteMode [
	self openModeFlag: FileOpenReadWriteFlag new.
]

{ #category : 'sharing' }
FileOptions >> beShared [
	self share: FileDenyNoneFlag new
]

{ #category : 'open mode flag' }
FileOptions >> beWriteMode [
	self openModeFlag: FileOpenWriteOnlyFlag new.
]

{ #category : 'sharing' }
FileOptions >> beWriteOnlyShared [
	self share: FileDenyReadFlag new
]

{ #category : 'file type' }
FileOptions >> binaryFileType [
	self fileType: FileBinaryTypeFlag new
]

{ #category : 'commands' }
FileOptions >> create [
	self hasNoReadOrWriteMode ifTrue: [^self signalOpenModeFlagNotDefined].
	self addCreateFlag
]

{ #category : 'convenience' }
FileOptions >> createRule [
	^self fileCreateFlag
		ifNil: [self store class openExistingRule]
		ifNotNil: [:option | option createRule]
]

{ #category : 'commands' }
FileOptions >> exclusive [
	self hasNoReadOrWriteMode ifTrue: [^self signalOpenModeFlagNotDefined].
	self addExclusiveFlag
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
FileOptions >> fileOpenFlagsTotalValue [
	^self fileOpenModeValue bitOr: self posixFlagsTotalValue
]

{ #category : 'posix flags' }
FileOptions >> fileOpenModeValue [
	^self openModeFlag posixFlagValue
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

{ #category : 'permissions' }
FileOptions >> groupAll [
	(self permissionsFlagForClass: FileGroupExecutePermissionFlag) ifNotNil: [:aFlag | self removePermissionFlag: aFlag].
	(self permissionsFlagForClass: FileGroupReadPermissionFlag) ifNotNil: [:aFlag | self removePermissionFlag: aFlag].
	(self permissionsFlagForClass: FileGroupWritePermissionFlag) ifNotNil: [:aFlag | self removePermissionFlag: aFlag].
	self addPermissionFlag: FileGroupAllPermissionFlag new
]

{ #category : 'permissions' }
FileOptions >> groupExecute [
	(self hasPermissionsFlag: FileGroupAllPermissionFlag) ifTrue: [^self].
	self addPermissionFlag: FileGroupExecutePermissionFlag new
]

{ #category : 'permissions' }
FileOptions >> groupRead [
	(self hasPermissionsFlag: FileGroupAllPermissionFlag) ifTrue: [^self].
	self addPermissionFlag: FileGroupReadPermissionFlag new
]

{ #category : 'permissions' }
FileOptions >> groupWrite [
	(self hasPermissionsFlag: FileGroupAllPermissionFlag) ifTrue: [^self].
	self addPermissionFlag: FileGroupWritePermissionFlag new
]

{ #category : 'file type' }
FileOptions >> gzipHighCompression [
	self fileType: FileGzipTypeFlag high
]

{ #category : 'file type' }
FileOptions >> gzipLowCompression [
	self fileType: FileGzipTypeFlag low
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
FileOptions >> hasNoReadOrWriteMode [
	^self openModeFlag isNil
]

{ #category : 'testing' }
FileOptions >> hasPermissionsFlag: anPosixFlagClass [
	^(self permissionsFlagForClass: anPosixFlagClass) notNil
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
	self beWriteMode.
	self addCreateFlag.
	self addAppendFlag
]

{ #category : 'initialization' }
FileOptions >> initializeRead [
	self beReadMode
]

{ #category : 'initialization' }
FileOptions >> initializeReadWrite [
	self beReadWriteMode
]

{ #category : 'initialization' }
FileOptions >> initializeWrite [
	self beWriteMode.
	self addCreateFlag.
	self addTruncateFlag
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
	^self openModeFlag isReadable
]

{ #category : 'testing' }
FileOptions >> isReadWritable [
	^self openModeFlag isReadable and: [self openModeFlag isWritable]
]

{ #category : 'testing' }
FileOptions >> isWritable [
	^self openModeFlag isWritable
]

{ #category : 'printing' }
FileOptions >> mode [
	| aStream |
	aStream := WriteStream on: String new.
	self printOpenModeStringOn: aStream.
	^aStream contents
]

{ #category : 'accessing' }
FileOptions >> openModeFlag [
	^openModeFlag
]

{ #category : 'accessing' }
FileOptions >> openModeFlag: aFileOpenMode [
	aFileOpenMode parent: self.
	openModeFlag :=  aFileOpenMode
]

{ #category : 'permissions' }
FileOptions >> otherAll [
	(self permissionsFlagForClass: FileOtherExecutePermissionFlag) ifNotNil: [:aFlag | self removePermissionFlag: aFlag].
	(self permissionsFlagForClass: FileOtherReadPermissionFlag) ifNotNil: [:aFlag | self removePermissionFlag: aFlag].
	(self permissionsFlagForClass: FileOtherWritePermissionFlag) ifNotNil: [:aFlag | self removePermissionFlag: aFlag].
	self addPermissionFlag: FileOtherAllPermissionFlag new
]

{ #category : 'permissions' }
FileOptions >> otherExecute [
	(self hasPermissionsFlag: FileOtherAllPermissionFlag) ifTrue: [^self].
	self addPermissionFlag: FileOtherExecutePermissionFlag new
]

{ #category : 'permissions' }
FileOptions >> otherRead [
	(self hasPermissionsFlag: FileOtherAllPermissionFlag) ifTrue: [^self].
	self addPermissionFlag: FileOtherReadPermissionFlag new
]

{ #category : 'permissions' }
FileOptions >> otherWrite [
	(self hasPermissionsFlag: FileOtherAllPermissionFlag) ifTrue: [^self].
	self addPermissionFlag: FileOtherWritePermissionFlag new
]

{ #category : 'permissions' }
FileOptions >> ownerAll [
	(self permissionsFlagForClass: FileOwnerExecutePermissionFlag) ifNotNil: [:aFlag | self removePermissionFlag: aFlag].
	(self permissionsFlagForClass: FileOwnerReadPermissionFlag) ifNotNil: [:aFlag | self removePermissionFlag: aFlag].
	(self permissionsFlagForClass: FileOwnerWritePermissionFlag) ifNotNil: [:aFlag | self removePermissionFlag: aFlag].
	self addPermissionFlag: FileOwnerAllPermissionFlag new
]

{ #category : 'permissions' }
FileOptions >> ownerExecute [
	(self hasPermissionsFlag: FileOwnerAllPermissionFlag) ifTrue: [^self].
	self addPermissionFlag: FileOwnerExecutePermissionFlag new
]

{ #category : 'permissions' }
FileOptions >> ownerRead [
	(self hasPermissionsFlag: FileOwnerAllPermissionFlag) ifTrue: [^self].
	self addPermissionFlag: FileOwnerReadPermissionFlag new
]

{ #category : 'permissions' }
FileOptions >> ownerWrite [
	(self hasPermissionsFlag: FileOwnerAllPermissionFlag) ifTrue: [^self].
	self addPermissionFlag: FileOwnerWritePermissionFlag new
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

{ #category : 'permissions' }
FileOptions >> permissionsFlagForClass: anPermissionFlagClass [
	^self permissionFlags detect: [:ea | ea isKindOf: anPermissionFlagClass] ifNone: [nil]
]

{ #category : 'posix flags' }
FileOptions >> posixFlagForClass: anPosixFlagClass [
	^self posixFlags detect: [:ea | ea isKindOf: anPosixFlagClass] ifNone: [nil]
]

{ #category : 'accessing' }
FileOptions >> posixFlags [
	^posixFlags
]

{ #category : 'accessing' }
FileOptions >> posixFlags: aCollection [
	posixFlags := aCollection
]

{ #category : 'posix flags' }
FileOptions >> posixFlagsTotalValue [
	^self posixFlags inject: 0 into: [:total :ea | total bitOr: ea posixFlagValue]
]

{ #category : 'printing' }
FileOptions >> printOn: aStream [
	super printOn: aStream.
	aStream
		nextPutAll: '(';
		nextPutAll: (self openModeFlag ifNil: ['nil'] ifNotNil: [:f | f posixName ifNil: ['nil']]);
		nextPutAll: ')'
]

{ #category : 'printing' }
FileOptions >> printOpenModeStringOn: aStream [

	self openModeFlag printOpenModeStringOn: aStream.
	self fileType ifNotNil: [:ft | ft printOpenModeStringOn: aStream.]
]

{ #category : 'commands' }
FileOptions >> read [
	self hasNoReadOrWriteMode ifTrue: [^self initializeRead].
	self isWritable ifTrue: [self beReadWriteMode]
]

{ #category : 'commands' }
FileOptions >> readWrite [
	self initializeReadWrite
]

{ #category : 'posix flags - adding and removing' }
FileOptions >> removeAppendFlag [
	self appendFlag ifNotNil: [:flag | self removePosixFlag: flag]
]

{ #category : 'permissions' }
FileOptions >> removePermissionFlag: aPermissionFlag [
	aPermissionFlag parent: nil.
	self permissionFlags remove: aPermissionFlag ifAbsent: []
]

{ #category : 'posix flags - adding and removing' }
FileOptions >> removePosixFlag: aPosixFlag [
	aPosixFlag parent: nil.
	self posixFlags remove: aPosixFlag ifAbsent: []
]

{ #category : 'posix flags - adding and removing' }
FileOptions >> removeTruncateFlag [
	self truncateFlag ifNotNil: [:flag | self removePosixFlag: flag]
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

{ #category : 'errors' }
FileOptions >> signalOpenModeFlagNotDefined [

	self error: 'Attempting to add an open flag without first defining the open mode of #read, #write or #readWrite'
]

{ #category : 'accessing' }
FileOptions >> store [
	^store ifNil: [
		self parent isNil ifTrue: [^FileSystem disk store].
		^self parent store]
]

{ #category : 'accessing' }
FileOptions >> store: aDiskStore [
	store := aDiskStore
]

{ #category : 'file type' }
FileOptions >> textFileType [
	self fileType: FileTextTypeFlag new
]

{ #category : 'commands' }
FileOptions >> truncate [
	self hasNoReadOrWriteMode ifTrue: [^self signalOpenModeFlagNotDefined].
	self addTruncateFlag
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

{ #category : 'commands' }
FileOptions >> write [
	self hasNoReadOrWriteMode ifTrue: [^self initializeWrite].
	self isReadable ifTrue: [self beReadWriteMode]
]
