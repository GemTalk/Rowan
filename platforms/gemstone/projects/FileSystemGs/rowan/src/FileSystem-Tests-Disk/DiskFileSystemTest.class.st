"
SUnit tests for the disk filesystem
"
Class {
	#name : 'DiskFileSystemTest',
	#superclass : 'FileSystemTest',
	#category : 'FileSystem-Tests-Disk'
}

{ #category : 'initialize-release' }
DiskFileSystemTest >> createFileSystem [
	^ FileSystem store: (DiskStore activeClass createDefault)
]

{ #category : 'tests-streams' }
DiskFileSystemTest >> testBinaryReadWriteStream [
	| reference stream |

	self markForCleanup: (reference := filesystem workingDirectory / 'griffle').
	self should: [ stream := reference readRawStream ] raise: FileDoesNotExistException.

	reference writeStreamDo: [:strm | strm nextPutAll: '0123456789' ].
	stream := reference binaryReadWriteStream.
	stream sizeBuffer: 8.

	stream nextPutAll: 'ABCD'.

	self assert: stream peek asCharacter equals: $4. 
"	self assert: stream position equals: 4."
	self assert: stream upToEnd asString equals: '456789'.
"	self assert: stream position equals: 10."
	self assert: stream atEnd.
	stream close.

	self assert: reference contents asString equals: 'ABCD456789'.
]

{ #category : 'tests' }
DiskFileSystemTest >> testDefaultWorkingDirectory [
	| ref x y |
	ref := filesystem workingDirectory.
	self assert: ((x := FileSystem fileClass currentWorkingDirectoryPath) beginsWith: (y := ref pathString))
]

{ #category : 'tests' }
DiskFileSystemTest >> testEqual [
	| other |
	other := self createFileSystem.
	self assert: filesystem = other
]

{ #category : 'tests' }
DiskFileSystemTest >> testIsDirectory [
	self assert: (filesystem isDirectory: FileLocator imageDirectory resolve path)
]

{ #category : 'tests' }
DiskFileSystemTest >> testIsDiskFileSystem [
	self assert: filesystem isDiskFileSystem.
	
]

{ #category : 'tests' }
DiskFileSystemTest >> testIsReadable [
	self assert: (filesystem store isReadable: FileLocator imageDirectory resolve path)
]

{ #category : 'tests' }
DiskFileSystemTest >> testIsWritable [
	| aReference |

	self deny: (filesystem store isWritable: FileLocator imageDirectory resolve path).

	aReference := FileLocator temp / 'plonk'.
    self markForCleanup: aReference resolve path.
	self deny: aReference exists.
	aReference writeStream close. 
	self assert: (filesystem store isWritable: aReference resolve path).
]

{ #category : 'tests' }
DiskFileSystemTest >> testMoveMemoryToDisk [
	"Test moving a file from the memory filesystem to the disk filesystem.
	This ensures that the copyAndDelete:to: is called correctly."
	| memfs out testString old new |
	[
		memfs := FileSystem memory.
		old := memfs / 'testMoveMemoryToDisk_old'.
		out := old writeStream.
		testString := 'To be moved to disk'.
		[ out nextPutAll: testString ] ensure: [ out close ].
		
		new := FileLocator imageDirectory / 'testMoveMemoryToDisk_new'.
		old moveTo: new.
		
		self deny: (memfs / 'testMoveMemoryToDisk_old') exists.
		self assert: new exists.
		self assert: new contents equals: testString.
	] ensure: [ 
		old ensureDelete.
		new ensureDelete.
	]
]

{ #category : 'tests' }
DiskFileSystemTest >> testOpenWithOptions [
	| aFileOptions  result reference|
	aFileOptions := FileOptions newWrite.
	reference := (FileLocator temp / 'tttt') resolve.
	self markForCleanup: reference.
	result := reference openWithOptions: aFileOptions.
	self assert: (result isKindOf: FileSystem fileClass)
]
