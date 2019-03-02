Class {
	#name : 'PosixErrorGroupWindowsTest',
	#superclass : 'PosixErrorGroupTest',
	#category : 'FileSystem-Tests-Disk-ErrorGroups'
}

{ #category : 'private' }
PosixErrorGroupWindowsTest >> nonExistantFileReference [
	"This results in a non sensical path which is what this test requires"
	^(FileSystem store: WindowsStore new) workingDirectory
]

{ #category : 'private' }
PosixErrorGroupWindowsTest >> setupFileSystem [
	fileSystem := FileSystem store: WindowsStore new
]

{ #category : 'tests - lookup file posix error class' }
PosixErrorGroupWindowsTest >> testChageModeErrorGroup [

	self setupForChageModeErrorGroup.
	fileReference := self nonExistantFileReference.

	posixErrorClass := errorGroup errorClassForErrorNumber: 1 fileReference: fileReference.
	self assert: posixErrorClass posixName equals: 'EIO'.

	posixErrorClass := errorGroup errorClassForErrorNumber: 2 fileReference: fileReference.
	self assert: posixErrorClass posixName equals: 'ENOENT'.

	posixErrorClass := errorGroup errorClassForErrorNumber: 123 fileReference: fileReference.
	self assert: posixErrorClass posixName equals: 'EACCES'.

	posixErrorClass := errorGroup errorClassForErrorNumber: 19 fileReference: fileReference.
	self assert: posixErrorClass posixName equals: 'EROFS'
]

{ #category : 'tests - lookup file posix error class' }
PosixErrorGroupWindowsTest >> testCloseDriectoryErrorGroup [

	self setupForCloseDirectoryErrorGroup.
	fileReference := fileSystem workingDirectory / 't2'.

	posixErrorClass := errorGroup errorClassForErrorNumber: 9999 fileReference: fileReference.
	self assert: posixErrorClass posixName equals: 'EIO'.

	posixErrorClass := errorGroup errorClassForErrorNumber: 6 fileReference: fileReference.
	self assert: posixErrorClass posixName equals: 'EBADF'
]

{ #category : 'tests - lookup file posix error class' }
PosixErrorGroupWindowsTest >> testCloseFileErrorGroup [

	self setupForCloseFileErrorGroup.
	fileReference := fileSystem workingDirectory / 't2'.

	posixErrorClass := errorGroup errorClassForErrorNumber: 9999 fileReference: fileReference.
	self assert: posixErrorClass posixName equals: 'EIO'.

	posixErrorClass := errorGroup errorClassForErrorNumber: 6 fileReference: fileReference.
	self assert: posixErrorClass posixName equals: 'EBADF'.

	self should: [errorGroup signalErrorNumber: 6 fileReference: fileReference] raise: FileBadFileDescriptorError
]

{ #category : 'tests - lookup file posix error class' }
PosixErrorGroupWindowsTest >> testCopyFileErrorGroup [
	| directory1 directory2 |
	
	self setupForCopyFileErrorGroup.
	directory1 := fileSystem workingDirectoryPath.
	directory2 := fileSystem workingDirectoryPath.
	oldFileReference := self createTemporaryFile: directory1 / 't1'	containing: 'griffle'.
	fileReference := fileSystem referenceTo: directory2 / 't2'.

	posixErrorClass := errorGroup errorClassForErrorNumber: 9999 newFileReference: fileReference oldFileReference: oldFileReference.
	self assert: posixErrorClass posixName equals: 'EIO'.

	posixErrorClass := errorGroup errorClassForErrorNumber: 3 newFileReference: fileReference oldFileReference: oldFileReference.
	self assert: posixErrorClass posixName equals: 'EACCES'.

	self isWindowsStoreCurrentStore ifTrue: [
		posixErrorClass := errorGroup errorClassForErrorNumber: 5 newFileReference: directory1 oldFileReference: oldFileReference.
		self assert: posixErrorClass posixName equals: 'EEXIST' description: 'Target file is a directory that already exists'.
		posixErrorClass := errorGroup errorClassForErrorNumber: 5 newFileReference: oldFileReference oldFileReference: oldFileReference.
		self assert: posixErrorClass posixName equals: 'EEXIST' description: 'Target file is a file that already exists'.
		posixErrorClass := errorGroup errorClassForErrorNumber: 5 newFileReference: fileReference oldFileReference: fileReference.
		self assert: posixErrorClass posixName equals: 'ENOENT' description: 'Source file is a does not exists'.
		posixErrorClass := errorGroup errorClassForErrorNumber: 5 newFileReference: fileReference oldFileReference: directory2.
		self assert: posixErrorClass posixName equals: 'EISDIR' description: 'Source file is actually a directory'].

	posixErrorClass := errorGroup errorClassForErrorNumber: 16 newFileReference: fileReference oldFileReference: oldFileReference.
	self assert: posixErrorClass posixName equals: 'EBUSY'.

	posixErrorClass := errorGroup errorClassForErrorNumber: 19 newFileReference: fileReference oldFileReference: oldFileReference.
	self assert: posixErrorClass posixName equals: 'EROFS'.

	posixErrorClass := errorGroup errorClassForErrorNumber: 21 newFileReference: fileReference oldFileReference: oldFileReference.
	self assert: posixErrorClass posixName equals: 'EACCES'.

	posixErrorClass := errorGroup errorClassForErrorNumber: 32 newFileReference: fileReference oldFileReference: oldFileReference.
	self assert: posixErrorClass posixName equals: 'EBUSY'.

	posixErrorClass := errorGroup errorClassForErrorNumber: 33 newFileReference: fileReference oldFileReference: oldFileReference.
	self assert: posixErrorClass posixName equals: 'EBUSY'.

	posixErrorClass := errorGroup errorClassForErrorNumber: 80 newFileReference: fileReference oldFileReference: oldFileReference.
	self assert: posixErrorClass posixName equals: 'EEXIST'.

	self isWindowsStoreCurrentStore ifTrue: [
		posixErrorClass := errorGroup errorClassForErrorNumber: 87 newFileReference: fileReference oldFileReference: fileReference.
		self assert: posixErrorClass posixName equals: 'EEXIST' description: 'Old file does not exist'.
		posixErrorClass := errorGroup errorClassForErrorNumber: 87 newFileReference: fileReference oldFileReference: oldFileReference.
		self assert: posixErrorClass posixName equals: 'EACCES' description: 'Old file exists but not able to access'].


	posixErrorClass := errorGroup errorClassForErrorNumber: 123 newFileReference: fileReference oldFileReference: oldFileReference.
	self assert: posixErrorClass posixName equals: 'EACCES'.

	posixErrorClass := errorGroup errorClassForErrorNumber: 161 newFileReference: fileReference oldFileReference: oldFileReference.
	self assert: posixErrorClass posixName equals: 'ENOENT'.

	posixErrorClass := errorGroup errorClassForErrorNumber: 183 newFileReference: fileReference oldFileReference: oldFileReference.
	self assert: posixErrorClass posixName equals: 'EEXIST'.
]

{ #category : 'tests - create file posix error' }
PosixErrorGroupWindowsTest >> testCreationOfFilePosixError [
	| fileOptions aPosixError |

	self setupForOpenFileErrorGroup.
	fileOptions := FileOptions read.
	fileReference := fileSystem workingDirectory / 't2'.

	errorGroup := OpenFileErrorGroup new.
	aPosixError := errorGroup errorForNumber: 3 fileReference: fileReference options: fileOptions.
	self assert: aPosixError class posixName equals: 'EACCES'.
	self assert: aPosixError reference equals: fileReference.
	self assert: aPosixError sourceReference isNil.
	self assert: aPosixError options equals: fileOptions.
	self assert: aPosixError errorGroupName = errorGroup class name asString.

	self should: [errorGroup signalErrorNumber: 3 fileReference: fileReference options: fileOptions] raise: FileAccessError.

	errorGroup := CopyFileErrorGroup new.
	aPosixError := errorGroup errorForNumber: 16 newFileReference: fileReference oldFileReference: fileSystem workingDirectory.
	self assert: aPosixError class posixName equals: 'EBUSY'.
	self assert: aPosixError reference equals: fileReference.
	self assert: aPosixError sourceReference equals: fileSystem workingDirectory.
	self assert: aPosixError options isNil.
	self assert: aPosixError errorGroupName = errorGroup class name asString.

	errorGroup := ChangeModeErrorGroup new.
	aPosixError := errorGroup errorForNumber: 123 fileReference: fileReference.
	self assert: aPosixError class posixName equals: 'EACCES'.
	self assert: aPosixError reference equals: fileReference.
	self assert: aPosixError sourceReference isNil.
	self assert: aPosixError options isNil.
	self assert: aPosixError errorGroupName = errorGroup class name asString
]

{ #category : 'tests - lookup file posix error class' }
PosixErrorGroupWindowsTest >> testFileControlErrorGroup [

	self setupForFileControlErrorGroup.
	fileReference := fileSystem workingDirectory / 't2'.

	posixErrorClass := errorGroup errorClassForErrorNumber: 9999 fileReference: fileReference.
	self assert: posixErrorClass posixName equals: 'EIO'.

	posixErrorClass := errorGroup errorClassForErrorNumber: 5 fileReference: fileReference.
	self assert: posixErrorClass posixName equals: 'EACCES'.

	posixErrorClass := errorGroup errorClassForErrorNumber: 6 fileReference: fileReference.
	self assert: posixErrorClass posixName equals: 'EBADF'.

	posixErrorClass := errorGroup errorClassForErrorNumber: 7 fileReference: fileReference.
	self assert: posixErrorClass posixName equals: 'EINVAL'
]

{ #category : 'tests - lookup file posix error class' }
PosixErrorGroupWindowsTest >> testFileSeekErrorGroup [

	self setupForFileSeekErrorGroup.
	fileReference := fileSystem workingDirectory / 't2'.

	posixErrorClass := errorGroup errorClassForErrorNumber: 9999 fileReference: fileReference.
	self assert: posixErrorClass posixName equals: 'EIO'.

	posixErrorClass := errorGroup errorClassForErrorNumber: 6 fileReference: fileReference.
	self assert: posixErrorClass posixName equals: 'EBADF'.

	posixErrorClass := errorGroup errorClassForErrorNumber: 87 fileReference: fileReference.
	self assert: posixErrorClass posixName equals: 'EINVAL'.

	posixErrorClass := errorGroup errorClassForErrorNumber: 131 fileReference: fileReference.
	self assert: posixErrorClass posixName equals: 'EINVAL'
]

{ #category : 'tests - lookup file posix error class' }
PosixErrorGroupWindowsTest >> testFileSizeErrorGroup [

	self setupForFileSizeErrorGroup.
	fileReference := fileSystem workingDirectory / 't2'.

	posixErrorClass := errorGroup errorClassForErrorNumber: 9999 fileReference: fileReference.
	self assert: posixErrorClass posixName equals: 'EIO'.

	posixErrorClass := errorGroup errorClassForErrorNumber: 6 fileReference: fileReference.
	self assert: posixErrorClass posixName equals: 'EBADF'
]

{ #category : 'tests - lookup file posix error class' }
PosixErrorGroupWindowsTest >> testFlushFileErrorGroup [

	self setupForFlushFileErrorGroup.
	fileReference := fileSystem workingDirectory / 't2'.

	posixErrorClass := errorGroup errorClassForErrorNumber: 9999 fileReference: fileReference.
	self assert: posixErrorClass posixName equals: 'EIO'.

	posixErrorClass := errorGroup errorClassForErrorNumber: 5 fileReference: fileReference.
	self assert: posixErrorClass posixName equals: 'EACCES'.

	posixErrorClass := errorGroup errorClassForErrorNumber: 6 fileReference: fileReference.
	self assert: posixErrorClass posixName equals: 'EBADF'
]

{ #category : 'tests - lookup file posix error class' }
PosixErrorGroupWindowsTest >> testLockFileErrorGroup [

	self setupForLockFileErrorGroup.
	fileReference := fileSystem workingDirectory / 't2'.

	posixErrorClass := errorGroup errorClassForErrorNumber: 9999 fileReference: fileReference.
	self assert: posixErrorClass posixName equals: 'EIO'.

	posixErrorClass := errorGroup errorClassForErrorNumber: 1 fileReference: fileReference.
	self assert: posixErrorClass posixName equals: 'EINVAL'.

	posixErrorClass := errorGroup errorClassForErrorNumber: 5 fileReference: fileReference.
	self assert: posixErrorClass posixName equals: 'EACCES'.

	posixErrorClass := errorGroup errorClassForErrorNumber: 6 fileReference: fileReference.
	self assert: posixErrorClass posixName equals: 'EBADF'.

	posixErrorClass := errorGroup errorClassForErrorNumber: 19 fileReference: fileReference.
	self assert: posixErrorClass posixName equals: 'EROFS'.

	posixErrorClass := errorGroup errorClassForErrorNumber: 32 fileReference: fileReference.
	self assert: posixErrorClass posixName equals: 'EBUSY'.

	posixErrorClass := errorGroup errorClassForErrorNumber: 33 fileReference: fileReference.
	self assert: posixErrorClass posixName equals: 'EBUSY'.
]

{ #category : 'tests - lookup file posix error class' }
PosixErrorGroupWindowsTest >> testOpenDirectoryErrorGroup [
	| fileOptions |

	self setupForOpenDirectoryErrorGroup.
	fileOptions := FileOptions read.
	fileReference := fileSystem workingDirectory / 't2'.

	posixErrorClass := errorGroup errorClassForErrorNumber: 9999 fileReference: fileReference.
	self assert: posixErrorClass posixName equals: 'EIO'.

	self isWindowsStoreCurrentStore ifTrue: [
		posixErrorClass := errorGroup errorClassForErrorNumber: 2 fileReference: fileReference.
		self assert: posixErrorClass posixName equals: 'ENOENT' description: 'Directory does not exist'].

	posixErrorClass := errorGroup errorClassForErrorNumber: 3 fileReference: fileReference.
	self assert: posixErrorClass posixName equals: 'EACCES'.

	posixErrorClass := errorGroup errorClassForErrorNumber: 5 fileReference: fileReference.
	self assert: posixErrorClass posixName equals: 'EACCES'.

	posixErrorClass := errorGroup errorClassForErrorNumber: 84 fileReference: fileReference.
	self assert: posixErrorClass posixName equals: 'ENFILE'.

	posixErrorClass := errorGroup errorClassForErrorNumber: 87 fileReference: fileReference.
	self assert: posixErrorClass posixName equals: 'EINVAL'.

	posixErrorClass := errorGroup errorClassForErrorNumber: 123 fileReference: fileReference.
	self assert: posixErrorClass posixName equals: 'EACCES'.

	posixErrorClass := errorGroup errorClassForErrorNumber: 161 fileReference: fileReference.
	self assert: posixErrorClass posixName equals: 'ENOENT'.

	posixErrorClass := errorGroup errorClassForErrorNumber: 267 fileReference: fileReference.
	self assert: posixErrorClass posixName equals: 'EACCES'
]

{ #category : 'tests - lookup file posix error class' }
PosixErrorGroupWindowsTest >> testOpenFileErrorGroup [
	| fileOptions |

	self setupForOpenFileErrorGroup.
	fileOptions := FileOptions read.
	fileReference := fileSystem workingDirectory / 't2'.

	posixErrorClass := errorGroup errorClassForErrorNumber: 9999 fileReference: fileReference options: fileOptions.
	self assert: posixErrorClass posixName equals: 'EIO'.

	self isWindowsStoreCurrentStore ifTrue: [
		posixErrorClass := errorGroup errorClassForErrorNumber: 2 fileReference: fileSystem workingDirectory options: fileOptions.
		self assert: posixErrorClass posixName equals: 'EACCES' description: 'Access Error - attempted to open a directory'.
		posixErrorClass := errorGroup errorClassForErrorNumber: 2 fileReference: fileSystem workingDirectory / 't1' options: fileOptions.
		self assert: posixErrorClass posixName equals: 'ENOENT' description: 'No file entry was found'].

	posixErrorClass := errorGroup errorClassForErrorNumber: 3 fileReference: fileReference options: fileOptions.
	self assert: posixErrorClass posixName equals: 'EACCES'.

	posixErrorClass := errorGroup errorClassForErrorNumber: 4 fileReference: fileReference options: fileOptions.
	self assert: posixErrorClass posixName equals: 'ENFILE'.

	self isWindowsStoreCurrentStore ifTrue: [
		posixErrorClass := errorGroup errorClassForErrorNumber: 5 fileReference: fileSystem workingDirectory options: fileOptions.
		self assert: posixErrorClass posixName equals: 'EACCES' description: 'File reference is a directory'.
		posixErrorClass := errorGroup errorClassForErrorNumber: 5 fileReference: fileReference options: FileOptions write.
		self assert: posixErrorClass posixName equals: 'EINVAL' description: 'Attempting to open with a truncate flag'.
		posixErrorClass := errorGroup errorClassForErrorNumber: 5 fileReference: fileReference options: FileOptions append.
		self assert: posixErrorClass posixName equals: 'EACCES' description: 'Attempting to create a file if does not exist'.
		posixErrorClass := errorGroup errorClassForErrorNumber: 5 fileReference: fileReference options: FileOptions read.
		self assert: posixErrorClass posixName equals: 'EACCES' description: 'Attempting to open a file with no access'.
		posixErrorClass := errorGroup errorClassForErrorNumber: 5 fileReference: fileReference options: FileOptions readWrite.
		self assert: posixErrorClass posixName equals: 'EINVAL' description: 'Catchall errors - no flags, file does not exist'].

	posixErrorClass := errorGroup errorClassForErrorNumber: 19 fileReference: fileReference options: fileOptions.
	self assert: posixErrorClass posixName equals: 'EROFS'.

	posixErrorClass := errorGroup errorClassForErrorNumber: 32 fileReference: fileReference options: fileOptions.
	self assert: posixErrorClass posixName equals: 'EBUSY'.

	posixErrorClass := errorGroup errorClassForErrorNumber: 33 fileReference: fileReference options: fileOptions.
	self assert: posixErrorClass posixName equals: 'EBUSY'.

	posixErrorClass := errorGroup errorClassForErrorNumber: 36 fileReference: fileReference options: fileOptions.
	self assert: posixErrorClass posixName equals: 'ENFILE'.

	posixErrorClass := errorGroup errorClassForErrorNumber: 80 fileReference: fileReference options: (FileOptions write addExclusiveFlag).
	self assert: posixErrorClass posixName equals: 'EACCES' description: 'Attempting to open with an exlusive flag'.

	posixErrorClass := errorGroup errorClassForErrorNumber: 80 fileReference: fileReference options: FileOptions write.
	self assert: posixErrorClass posixName equals: 'EEXIST' description: 'Attempting to open without an exlusive flag'.

	self isWindowsStoreCurrentStore ifTrue: [
		posixErrorClass := errorGroup errorClassForErrorNumber: 87 fileReference: fileSystem workingDirectory options: fileOptions.
		self assert: posixErrorClass posixName equals: 'EACCES' description: 'File reference is a directory'.
		posixErrorClass := errorGroup errorClassForErrorNumber: 87 fileReference: fileReference options: FileOptions write.
		self assert: posixErrorClass posixName equals: 'EINVAL' description: 'Attempting to open with a truncate flag'.
		posixErrorClass := errorGroup errorClassForErrorNumber: 87 fileReference: fileReference options: FileOptions append.
		self assert: posixErrorClass posixName equals: 'EACCES' description: 'Attempting to create a file if does not exist'.
		posixErrorClass := errorGroup errorClassForErrorNumber: 87 fileReference: fileReference options: FileOptions read.
		self assert: posixErrorClass posixName equals: 'EACCES' description: 'Attempting to open a file with no access'.
		posixErrorClass := errorGroup errorClassForErrorNumber: 87 fileReference: fileReference options: FileOptions readWrite.
		self assert: posixErrorClass posixName equals: 'EINVAL' description: 'Catchall errors - no flags, file does not exist'].

	posixErrorClass := errorGroup errorClassForErrorNumber: 123 fileReference: fileReference options: fileOptions.
	self assert: posixErrorClass posixName equals: 'EACCES'.

	posixErrorClass := errorGroup errorClassForErrorNumber: 161 fileReference: fileReference options: fileOptions.
	self assert: posixErrorClass posixName equals: 'EACCES'
]

{ #category : 'tests - lookup file posix error class' }
PosixErrorGroupWindowsTest >> testReadDirectoryErrorGroup [

	self setupForReadDirectoryGroup.
	fileReference := fileSystem referenceTo: fileSystem workingDirectoryPath / 't2'.

	posixErrorClass := errorGroup errorClassForErrorNumber: 9999 fileReference: fileReference.
	self assert: posixErrorClass posixName equals: 'EIO'.

	posixErrorClass := errorGroup errorClassForErrorNumber: 3 fileReference: fileReference.
	self assert: posixErrorClass posixName equals: 'EACCES'
]

{ #category : 'tests - lookup file posix error class' }
PosixErrorGroupWindowsTest >> testReadFileErrorGroup [

	self setupForReadFileErrorGroup.
	fileReference := fileSystem referenceTo: fileSystem workingDirectoryPath / 't2'.

	posixErrorClass := errorGroup errorClassForErrorNumber: 9999 fileReference: fileReference.
	self assert: posixErrorClass posixName equals: 'EIO'.

	posixErrorClass := errorGroup errorClassForErrorNumber: 5 fileReference: fileReference.
	self assert: posixErrorClass posixName equals: 'EACCES'.

	posixErrorClass := errorGroup errorClassForErrorNumber: 6 fileReference: fileReference.
	self assert: posixErrorClass posixName equals: 'EBADF'.

	posixErrorClass := errorGroup errorClassForErrorNumber: 33 fileReference: fileReference.
	self assert: posixErrorClass posixName equals: 'EBUSY'.

	posixErrorClass := errorGroup errorClassForErrorNumber: 112 fileReference: fileReference.
	self assert: posixErrorClass posixName equals: 'EACCES'
]

{ #category : 'tests - lookup file posix error class' }
PosixErrorGroupWindowsTest >> testRemoveFileErrorGroup [

	self setupForRemoveFileErrorGroup.
	fileReference := fileSystem referenceTo: fileSystem workingDirectoryPath / 't2'.

	posixErrorClass := errorGroup errorClassForErrorNumber: 9999 fileReference: fileReference.
	self assert: posixErrorClass posixName equals: 'EIO'.

	self isWindowsStoreCurrentStore ifTrue: [
		posixErrorClass := errorGroup errorClassForErrorNumber: 2 fileReference: fileSystem workingDirectory.
		self assert: posixErrorClass posixName equals: 'EISDIR' description: 'Access Error - attempted to open a directory'.
		posixErrorClass := errorGroup errorClassForErrorNumber: 2 fileReference: fileSystem workingDirectory / 't1'.
		self assert: posixErrorClass posixName equals: 'ENOENT' description: 'No file entry was found'].

	posixErrorClass := errorGroup errorClassForErrorNumber: 3 fileReference: fileReference.
	self assert: posixErrorClass posixName equals: 'EACCES' description: 'File reference is a directory'.

	self isWindowsStoreCurrentStore ifTrue: [
		posixErrorClass := errorGroup errorClassForErrorNumber: 5 fileReference: fileSystem workingDirectory.
		self assert: posixErrorClass posixName equals: 'EISDIR' description: 'Access Error - attempted to open a directory'.
		posixErrorClass := errorGroup errorClassForErrorNumber: 5 fileReference: fileSystem workingDirectory / 't1'.
		self assert: posixErrorClass posixName equals: 'ENOENT' description: 'No file entry was found'].

	posixErrorClass := errorGroup errorClassForErrorNumber: 19 fileReference: fileReference.
	self assert: posixErrorClass posixName equals: 'EROFS'.

	posixErrorClass := errorGroup errorClassForErrorNumber: 145 fileReference: fileReference.
	self assert: posixErrorClass posixName equals: 'EEXIST'.

	posixErrorClass := errorGroup errorClassForErrorNumber: 161 fileReference: fileReference.
	self assert: posixErrorClass posixName equals: 'ENOENT'
]

{ #category : 'tests - lookup file posix error class' }
PosixErrorGroupWindowsTest >> testRenameFileErrorGroup [

	| directory1 directory2 |

	directory1 := fileSystem workingDirectoryPath.
	directory2 := fileSystem workingDirectoryPath.
	oldFileReference := self createTemporaryFile: directory1 / 't1'	containing: 'griffle'.
	fileReference := fileSystem referenceTo: directory2 / 't2'.

	self setupForRenameFileErrorGroup.
	fileReference := fileSystem workingDirectory / 't2'.

	posixErrorClass := errorGroup errorClassForErrorNumber: 9999 newFileReference: fileReference oldFileReference: fileReference.
	self assert: posixErrorClass posixName equals: 'EIO'.

	self should: [errorGroup signalErrorNumber: 9999 newFileReference: fileReference oldFileReference: fileReference] raise: FileIOError.

	self isWindowsStoreCurrentStore ifTrue: [
		posixErrorClass := errorGroup errorClassForErrorNumber: 2 newFileReference: fileReference oldFileReference: fileReference.
		self assert: posixErrorClass posixName equals: 'ENOENT' description: 'New file does not exist'.
		posixErrorClass := errorGroup errorClassForErrorNumber: 2 newFileReference: oldFileReference oldFileReference: fileReference.
		self assert: posixErrorClass posixName equals: 'ENOENT' description: 'Old file does not exist'.
		posixErrorClass := errorGroup errorClassForErrorNumber: 2 newFileReference: oldFileReference oldFileReference: oldFileReference.
		self assert: posixErrorClass posixName equals: 'EEXIST' description: 'New file does not exist - it should not exist for rename to work'.
		posixErrorClass := errorGroup errorClassForErrorNumber: 2 newFileReference: oldFileReference oldFileReference: oldFileReference.
		self assert: posixErrorClass posixName equals: 'EXDEV' description: 'No file entry was found'].

	self isWindowsStoreCurrentStore ifTrue: [
		posixErrorClass := errorGroup errorClassForErrorNumber: 3 newFileReference: oldFileReference oldFileReference: fileReference.
		self assert: posixErrorClass posixName equals: 'EEXIST' description: 'New file already exists'.
		posixErrorClass := errorGroup errorClassForErrorNumber: 3 newFileReference: fileReference oldFileReference: fileSystem workingDirectory.
		self assert: posixErrorClass posixName equals: 'EISDIR' description: 'Old file is a directory'.
		posixErrorClass := errorGroup errorClassForErrorNumber: 5 newFileReference: fileReference oldFileReference: oldFileReference.
		self assert: posixErrorClass posixName equals: 'EACCES' description: 'Catach all'].

	self isWindowsStoreCurrentStore ifTrue: [
		posixErrorClass := errorGroup errorClassForErrorNumber: 5 newFileReference: oldFileReference oldFileReference: fileReference.
		self assert: posixErrorClass posixName equals: 'EEXIST' description: 'New file already exists'.
		posixErrorClass := errorGroup errorClassForErrorNumber: 5 newFileReference: fileReference oldFileReference: fileSystem workingDirectory.
		self assert: posixErrorClass posixName equals: 'EISDIR' description: 'Old file is a directory'.
		posixErrorClass := errorGroup errorClassForErrorNumber: 5 newFileReference: fileReference oldFileReference: oldFileReference.
		self assert: posixErrorClass posixName equals: 'EACCES' description: 'Catach all'].

	self isWindowsStoreCurrentStore ifTrue: [
		posixErrorClass := errorGroup errorClassForErrorNumber: 17 newFileReference: fileSystem workingDirectory oldFileReference: fileReference.
		self assert: posixErrorClass posixName equals: 'EISDIR'.
		posixErrorClass := errorGroup errorClassForErrorNumber: 17 newFileReference: fileSystem workingDirectory / 't1' oldFileReference: fileReference.
		self assert: posixErrorClass posixName equals: 'EXDEV'].

	posixErrorClass := errorGroup errorClassForErrorNumber: 19 newFileReference: fileReference oldFileReference: fileReference.
	self assert: posixErrorClass posixName equals: 'EROFS'.

	self isWindowsStoreCurrentStore ifTrue: [
		posixErrorClass := errorGroup errorClassForErrorNumber: 87 newFileReference: oldFileReference oldFileReference: fileReference.
		self assert: posixErrorClass posixName equals: 'EEXIST' description: 'New file already exists'.
		posixErrorClass := errorGroup errorClassForErrorNumber: 87 newFileReference: fileReference oldFileReference: fileSystem workingDirectory.
		self assert: posixErrorClass posixName equals: 'EISDIR' description: 'Old file is a directory'.
		posixErrorClass := errorGroup errorClassForErrorNumber: 87 newFileReference: fileReference oldFileReference: fileReference.
		self assert: posixErrorClass posixName equals: 'ENOENT' description: 'Old file does not exist'.
		posixErrorClass := errorGroup errorClassForErrorNumber: 87 newFileReference: fileReference oldFileReference: oldFileReference.
		self assert: posixErrorClass posixName equals: 'EACCES' description: 'Catach all'].

	posixErrorClass := errorGroup errorClassForErrorNumber: 123 newFileReference: fileReference oldFileReference: fileReference.
	self assert: posixErrorClass posixName equals: 'EACCES'.

	posixErrorClass := errorGroup errorClassForErrorNumber: 161 newFileReference: fileReference oldFileReference: fileReference.
	self assert: posixErrorClass posixName equals: 'ENOENT'.

	posixErrorClass := errorGroup errorClassForErrorNumber: 183 newFileReference: fileReference oldFileReference: fileReference.
	self assert: posixErrorClass posixName equals: 'EEXIST'
]

{ #category : 'tests - lookup file posix error class' }
PosixErrorGroupWindowsTest >> testTouchFileErrorGroup [

	self setupForTouchFileErrorGroup.
	fileReference := fileSystem workingDirectory / 't2'.

	posixErrorClass := errorGroup errorClassForErrorNumber: 9999 fileReference: fileReference.
	self assert: posixErrorClass posixName equals: 'EIO'.

	posixErrorClass := errorGroup errorClassForErrorNumber: 2 fileReference: fileReference.
	self assert: posixErrorClass posixName equals: 'ENOENT'.

	posixErrorClass := errorGroup errorClassForErrorNumber: 3 fileReference: fileReference.
	self assert: posixErrorClass posixName equals: 'ENOENT'.

	posixErrorClass := errorGroup errorClassForErrorNumber: 4 fileReference: fileReference.
	self assert: posixErrorClass posixName equals: 'ENFILE'.

	posixErrorClass := errorGroup errorClassForErrorNumber: 5 fileReference: fileReference.
	self assert: posixErrorClass posixName equals: 'EACCES'.

	posixErrorClass := errorGroup errorClassForErrorNumber: 19 fileReference: fileReference.
	self assert: posixErrorClass posixName equals: 'EROFS'.

	posixErrorClass := errorGroup errorClassForErrorNumber: 123 fileReference: fileReference.
	self assert: posixErrorClass posixName equals: 'EACCES'.

	posixErrorClass := errorGroup errorClassForErrorNumber: 161 fileReference: fileReference.
	self assert: posixErrorClass posixName equals: 'ENOENT'.
]

{ #category : 'tests - lookup file posix error class' }
PosixErrorGroupWindowsTest >> testUnlockFileErrorGroup [

	self setupForUnlockFileErrorGroup.
	fileReference := fileSystem workingDirectory / 't2'.

	posixErrorClass := errorGroup errorClassForErrorNumber: 9999 fileReference: fileReference.
	self assert: posixErrorClass posixName equals: 'EIO'.

	posixErrorClass := errorGroup errorClassForErrorNumber: 1 fileReference: fileReference.
	self assert: posixErrorClass posixName equals: 'EINVAL'.

	posixErrorClass := errorGroup errorClassForErrorNumber: 5 fileReference: fileReference.
	self assert: posixErrorClass posixName equals: 'EACCES'.

	posixErrorClass := errorGroup errorClassForErrorNumber: 6 fileReference: fileReference.
	self assert: posixErrorClass posixName equals: 'EBADF'.

	posixErrorClass := errorGroup errorClassForErrorNumber: 19 fileReference: fileReference.
	self assert: posixErrorClass posixName equals: 'EROFS'.

	posixErrorClass := errorGroup errorClassForErrorNumber: 32 fileReference: fileReference.
	self assert: posixErrorClass posixName equals: 'EBUSY'.

	posixErrorClass := errorGroup errorClassForErrorNumber: 33 fileReference: fileReference.
	self assert: posixErrorClass posixName equals: 'EBUSY'.

	posixErrorClass := errorGroup errorClassForErrorNumber: 158 fileReference: fileReference.
	self assert: posixErrorClass posixName equals: 'EINVAL'.
]

{ #category : 'tests - lookup file posix error class' }
PosixErrorGroupWindowsTest >> testWritingErrorGroup [

	self setupForWritingErrorGroup.
	fileReference := fileSystem workingDirectory / 't2'.

	posixErrorClass := errorGroup errorClassForErrorNumber: 9999 fileReference: fileReference.
	self assert: posixErrorClass posixName equals: 'EIO'.

	posixErrorClass := errorGroup errorClassForErrorNumber: 5 fileReference: fileReference.
	self assert: posixErrorClass posixName equals: 'EACCES'.

	posixErrorClass := errorGroup errorClassForErrorNumber: 6 fileReference: fileReference.
	self assert: posixErrorClass posixName equals: 'EBADF'.

	posixErrorClass := errorGroup errorClassForErrorNumber: 19 fileReference: fileReference.
	self assert: posixErrorClass posixName equals: 'EROFS'.

	posixErrorClass := errorGroup errorClassForErrorNumber: 33 fileReference: fileReference.
	self assert: posixErrorClass posixName equals: 'EBUSY'.

	posixErrorClass := errorGroup errorClassForErrorNumber: 112 fileReference: fileReference.
	self assert: posixErrorClass posixName equals: 'EACCES'.
]
