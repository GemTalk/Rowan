Class {
	#name : 'PosixErrorGroupTest',
	#superclass : 'TestCase',
	#instVars : [
		'fileReference',
		'errorGroup',
		'posixErrorClass',
		'fileSystem',
		'oldFileReference',
		'filesToDelete',
		'directoriesToDelete'
	],
	#category : 'FileSystem-Tests-Disk-ErrorGroups'
}

{ #category : 'private' }
PosixErrorGroupTest >> cleanupPaths [

	filesToDelete
		select: [ :path | fileSystem exists: path ]
		thenDo: [ :path | fileSystem delete: path ].
	directoriesToDelete
		select: [ :path | fileSystem exists: path ]
		thenDo: [ :path | fileSystem delete: path ]
]

{ #category : 'private' }
PosixErrorGroupTest >> createTemporaryDirectory: aPath [	

	| aFileReference |

	aFileReference := fileSystem referenceTo: aPath.
	self markForCleanupDirectory: aPath.
	fileSystem store createDirectory: aPath.
	^aFileReference
]

{ #category : 'private' }
PosixErrorGroupTest >> createTemporaryFile: aPath	containing: aString [

	| aFileReference |

	aFileReference := fileSystem referenceTo: aPath.
	self markForCleanupFile: aFileReference.
	aFileReference writeStreamDo: [ :ws | ws nextPutAll: aString ].
	^aFileReference
]

{ #category : 'testing' }
PosixErrorGroupTest >> isWindowsStoreCurrentStore [

	^FileSystem disk store class name == #WindowsStore
]

{ #category : 'private' }
PosixErrorGroupTest >> markForCleanupDirectory: aFileReference [

	directoriesToDelete add: aFileReference
]

{ #category : 'private' }
PosixErrorGroupTest >> markForCleanupFile: aFileReference [

	filesToDelete add: aFileReference
]

{ #category : 'Running' }
PosixErrorGroupTest >> setUp [
	super setUp.
	self setupFileSystem.
	filesToDelete := OrderedCollection new.
	directoriesToDelete := OrderedCollection new.
]

{ #category : 'private' }
PosixErrorGroupTest >> setupFileSystem [
	self subclassResponsibility
]

{ #category : 'private' }
PosixErrorGroupTest >> setupForChageModeErrorGroup [

	errorGroup := ChangeModeErrorGroup new
]

{ #category : 'private' }
PosixErrorGroupTest >> setupForCloseDirectoryErrorGroup [

	errorGroup := CloseDirectoryErrorGroup new
]

{ #category : 'private' }
PosixErrorGroupTest >> setupForCloseFileErrorGroup [

	errorGroup := CloseFileErrorGroup new
]

{ #category : 'private' }
PosixErrorGroupTest >> setupForCopyFileErrorGroup [

	errorGroup := CopyFileErrorGroup new
]

{ #category : 'private' }
PosixErrorGroupTest >> setupForFileControlErrorGroup [

	errorGroup := FileControlErrorGroup new
]

{ #category : 'private' }
PosixErrorGroupTest >> setupForFileSeekErrorGroup [

	errorGroup := FileSeekErrorGroup new
]

{ #category : 'private' }
PosixErrorGroupTest >> setupForFileSizeErrorGroup [

	errorGroup := FileSizeErrorGroup new
]

{ #category : 'private' }
PosixErrorGroupTest >> setupForFlushFileErrorGroup [

	errorGroup := FlushFileErrorGroup new
]

{ #category : 'private' }
PosixErrorGroupTest >> setupForLockFileErrorGroup [

	errorGroup := LockFileErrorGroup new
]

{ #category : 'private' }
PosixErrorGroupTest >> setupForOpenDirectoryErrorGroup [

	errorGroup := OpenDirectoryErrorGroup new
]

{ #category : 'private' }
PosixErrorGroupTest >> setupForOpenFileErrorGroup [

	errorGroup := OpenFileErrorGroup new
]

{ #category : 'private' }
PosixErrorGroupTest >> setupForReadDirectoryGroup [

	errorGroup := ReadDirectoryErrorGroup new
]

{ #category : 'private' }
PosixErrorGroupTest >> setupForReadFileErrorGroup [

	errorGroup := ReadFileErrorGroup new
]

{ #category : 'private' }
PosixErrorGroupTest >> setupForRemoveFileErrorGroup [

	errorGroup := RemoveFileErrorGroup new
]

{ #category : 'private' }
PosixErrorGroupTest >> setupForRenameFileErrorGroup [

	errorGroup := RenameFileErrorGroup new
]

{ #category : 'private' }
PosixErrorGroupTest >> setupForTouchFileErrorGroup [

	errorGroup := TouchFileErrorGroup new
]

{ #category : 'private' }
PosixErrorGroupTest >> setupForUnlockFileErrorGroup [

	errorGroup := UnlockFileErrorGroup new
]

{ #category : 'private' }
PosixErrorGroupTest >> setupForWritingErrorGroup [

	errorGroup := WritingErrorGroup new
]

{ #category : 'Running' }
PosixErrorGroupTest >> tearDown [
	self cleanupPaths.
	super tearDown
]
