"
SUnit tests for the disk filesystem
"
Class {
	#name : 'DiskFileSystemTest',
	#superclass : 'FileSystemTest',
	#category : 'FileSystem-Tests-Disk'
}

{ #category : 'private' }
DiskFileSystemTest >> _deleteDirectoryWithSymbolicLink: createSymbolLink [
	"from 50514.topaz ... internal bug 50514"

	| parentDir xxxDir allEntries target setupBlock testBlock commandLine assertionError reference |
	reference := filesystem workingDirectory / '50514'.
	reference ensureCreateDirectory.
	self markForCleanup: reference pathString.
	assertionError := false.
	parentDir := '50514' asFileReference.
	target := 'target_50514' asFileReference.
	target ensureCreateFile.
	self markForCleanup: target pathString.
	setupBlock := [ 
	xxxDir := parentDir / 'xxx'.
	xxxDir ensureCreateDirectory.
	(xxxDir / 'yyy') ensureCreateFile.
	(xxxDir / 'zzz') ensureCreateDirectory ].
	setupBlock value.
	testBlock := [ :val1 :val2 | 
	| ans |
	allEntries := parentDir allEntries collect: [ :each | each fullName ].
	(ans := allEntries size) = val1
		ifFalse: [ 
			assertionError
				ifTrue: [ self error: 'woops 1: ' , ans printString ] ].
	xxxDir ensureDeleteAll.
	allEntries := parentDir allEntries collect: [ :each | each fullName ].
	(ans := allEntries size) = val2
		ifFalse: [ 
			assertionError
				ifTrue: [ self error: 'woops 2: ' , ans printString ] ] ].
	testBlock value: 4 value: 1.
	setupBlock value.
	createSymbolLink
		ifTrue: [ 
			commandLine := 'set -e;  cd ' , xxxDir fullName , '; ln -s ../'
				, target basename , ' .'.
			Rowan gitTools performOnServer: commandLine.
			self assert: (xxxDir / target basename) isSymlink ].
	testBlock value: 5 value: 1
]

{ #category : 'initialize-release' }
DiskFileSystemTest >> createFileSystem [
	^ FileSystem store: (DiskStore activeClass createDefault)
]

{ #category : 'tests' }
DiskFileSystemTest >> testDefaultWorkingDirectory [
	| ref x y |
	ref := filesystem workingDirectory.
	self assert: ((x := GsFile _expandEnvVariable: 'PWD' isClient:false) beginsWith: (y := ref pathString))
]

{ #category : 'tests' }
DiskFileSystemTest >> testDeleteDirectoryWithNoSymbolicLink [
	"from 50514.topaz ... internal bug 50514"

	self _deleteDirectoryWithSymbolicLink: false
]

{ #category : 'tests' }
DiskFileSystemTest >> testDeleteDirectoryWithSymbolicLink [
	"from 50514.topaz ... internal bug 50514"

	self _deleteDirectoryWithSymbolicLink: true
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
