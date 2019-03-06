Class {
	#name : 'PosixErrorGroupUnixTest',
	#superclass : 'PosixErrorGroupTest',
	#category : 'FileSystem-Tests-Base-ErrorGroups'
}

{ #category : 'private' }
PosixErrorGroupUnixTest >> setupFileSystem [
	fileSystem := FileSystem store: UnixStore new
]

{ #category : 'tests - lookup file posix error class' }
PosixErrorGroupUnixTest >> testChageModeErrorGroup [

	self setupForChageModeErrorGroup.
	fileReference := fileSystem workingDirectory.

	self should: [ errorGroup errorClassForErrorNumber: 9999 fileReference: fileReference ] raise: Error description: 'Error name is not in valid error list'.

	posixErrorClass := errorGroup errorClassForErrorNumber: FileIOError defaultErrorNumber fileReference: fileReference.
	self assert: posixErrorClass posixName equals: 'EIO'.

	posixErrorClass := errorGroup errorClassForErrorNumber: FileAccessError defaultErrorNumber fileReference: fileReference.
	self assert: posixErrorClass posixName equals: 'EACCES'.
	
	posixErrorClass := errorGroup errorClassForErrorNumber: FileReadOnlyFileSystemError defaultErrorNumber fileReference: fileReference.
	self assert: posixErrorClass posixName equals: 'EROFS'.

	posixErrorClass := errorGroup errorClassForErrorNumber: FilePermissionDeniedError defaultErrorNumber fileReference: fileReference.
	self assert: posixErrorClass posixName equals: 'EPERM'
]

{ #category : 'tests - lookup file posix error class' }
PosixErrorGroupUnixTest >> testCopyFileErrorGroup [
	| directory1 directory2 |

	self setupForCopyFileErrorGroup.
	directory1 := self createTemporaryDirectory: Path * 'plonk1'.
	directory2 := self createTemporaryDirectory: Path * 'plonk2'.
	oldFileReference := self createTemporaryFile: directory1 path / 't1'	containing: 'griffle'.
	fileReference := fileSystem referenceTo: directory2 path / 't2'.

	self should: [ errorGroup errorClassForErrorNumber: 9999 fileReference: fileReference ] raise: Error description: 'Error name is not in valid error list'.

	posixErrorClass := errorGroup errorClassForErrorNumber: FileIOError defaultErrorNumber fileReference: fileReference.
	self assert: posixErrorClass posixName equals: 'EIO'.

	posixErrorClass := errorGroup errorClassForErrorNumber: 13 newFileReference: fileReference oldFileReference: oldFileReference.
	self assert: posixErrorClass posixName equals: 'EACCES'.
	
"	posixErrorClass := errorGroup errorClassForErrorNumber: FileReadOnlyFileSystemError defaultErrorNumber fileReference: fileReference.
	self assert: posixErrorClass posixName equals: 'EROFS'.

	posixErrorClass := errorGroup errorClassForErrorNumber: FilePermissionDeniedError defaultErrorNumber fileReference: fileReference.
	self assert: posixErrorClass posixName equals: 'EPERM'"
]

{ #category : 'tests - create file posix error' }
PosixErrorGroupUnixTest >> testCreationOfFilePosixError [
	| fileOptions aPosixError |

	self setupForOpenFileErrorGroup.
	fileOptions := FileOptions read.
	fileReference := fileSystem workingDirectory / 't2'.

	errorGroup := OpenFileErrorGroup new.
	aPosixError := errorGroup errorForNumber: FileExistsError defaultErrorNumber fileReference: fileReference options: fileOptions.
	self assert: aPosixError class posixName equals: 'EEXIST'.
	self assert: aPosixError reference equals: fileReference.
	self assert: aPosixError sourceReference isNil.
	self assert: aPosixError options equals: fileOptions.
	self assert: aPosixError errorGroupName = errorGroup class name asString.

	errorGroup := CopyFileErrorGroup new.
	aPosixError := errorGroup errorForNumber: FileExistsError defaultErrorNumber newFileReference: fileReference oldFileReference: fileSystem workingDirectory.
	self assert: aPosixError class posixName equals: 'EEXIST'.
	self assert: aPosixError reference equals: fileReference.
	self assert: aPosixError sourceReference equals: fileSystem workingDirectory.
	self assert: aPosixError options isNil.
	self assert: aPosixError errorGroupName = errorGroup class name asString.

	errorGroup := ChangeModeErrorGroup new.
	aPosixError := errorGroup errorForNumber: FileAccessError defaultErrorNumber fileReference: fileReference.
	self assert: aPosixError class posixName equals: 'EACCES'.
	self assert: aPosixError reference equals: fileReference.
	self assert: aPosixError sourceReference isNil.
	self assert: aPosixError options isNil.
	self assert: aPosixError errorGroupName = errorGroup class name asString
]

{ #category : 'tests - lookup file posix error class' }
PosixErrorGroupUnixTest >> testOpenFileErrorGroup [
	| fileOptions |

	self setupForOpenFileErrorGroup.
	fileOptions := FileOptions read.
"	directory1 := self createTemporaryDirectory: Path * 'plonk1'.
	oldFileReference := self createTemporaryFile: directory1 path / 't1'	containing: 'griffle'."
	fileReference := fileSystem workingDirectory / 't2'.

	self should: [ errorGroup errorClassForErrorNumber: 9999 fileReference: fileReference ] raise: Error description: 'Error name is not in valid error list'.

	posixErrorClass := errorGroup errorClassForErrorNumber: FileIOError defaultErrorNumber fileReference: fileReference.
	self assert: posixErrorClass posixName equals: 'EIO'.

	posixErrorClass := errorGroup errorClassForErrorNumber: FileExistsError defaultErrorNumber fileReference: fileReference options: fileOptions.
	self assert: posixErrorClass posixName equals: 'EEXIST'.

	posixErrorClass := errorGroup errorClassForErrorNumber: FileReadOnlyFileSystemError defaultErrorNumber fileReference: fileReference options: fileOptions.
	self assert: posixErrorClass posixName equals: 'EROFS'.

	posixErrorClass := errorGroup errorClassForErrorNumber: FileAccessError defaultErrorNumber fileReference: fileReference options: fileOptions.
	self assert: posixErrorClass posixName equals: 'EACCES'.

	posixErrorClass := errorGroup errorClassForErrorNumber: FileBusyError defaultErrorNumber fileReference: fileReference options: fileOptions.
	self assert: posixErrorClass posixName equals: 'EBUSY'.

	posixErrorClass := errorGroup errorClassForErrorNumber: FileNoSpaceError defaultErrorNumber fileReference: fileReference options: fileOptions.
	self assert: posixErrorClass posixName equals: 'ENOSPC'.

	posixErrorClass := errorGroup errorClassForErrorNumber: FileNoEntryError defaultErrorNumber fileReference: fileReference options: fileOptions.
	self assert: posixErrorClass posixName equals: 'ENOENT'.

	posixErrorClass := errorGroup errorClassForErrorNumber: FileMaxFilesOpenError defaultErrorNumber fileReference: fileReference options: fileOptions.
	self assert: posixErrorClass posixName equals: 'ENFILE'.

	posixErrorClass := errorGroup errorClassForErrorNumber:  FileInvalidOptionError defaultErrorNumber fileReference: fileReference options: fileOptions.
	self assert: posixErrorClass posixName equals: 'EINVAL'
]
