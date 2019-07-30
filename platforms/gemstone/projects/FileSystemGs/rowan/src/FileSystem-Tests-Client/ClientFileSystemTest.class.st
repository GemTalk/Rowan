Class {
	#name : 'ClientFileSystemTest',
	#superclass : 'FileSystemTest',
	#category : 'FileSystem-Tests-Client'
}

{ #category : 'tests' }
ClientFileSystemTest >> createFileSystem [
	^ FileSystem store: ClientStore new
]

{ #category : 'tests' }
ClientFileSystemTest >> testChildrenAt [
	| directory entries |
	directory := Path * 'plonk'.
	
	filesystem createDirectory: directory.
	filesystem createDirectory: directory / 'griffle'.
	filesystem createDirectory: directory / 'bint'.
	
	self markForCleanup: directory / 'griffle'.
	self markForCleanup: directory / 'bint'.
	self markForCleanup: directory.
	
	self should: [filesystem childrenAt: directory] raise: Error.

"	self assert: entries size = 2.
	entries do: [ :ea | 
		self assert: (ea isKindOf: Path).
		self assert: ea parent = (filesystem resolve: directory).
		self assert: (#('griffle' 'bint' ) includes: ea basename) ]"
]

{ #category : 'tests' }
ClientFileSystemTest >> testChildrenSorting [
	| directory sorted |
	
	directory := Path * 'plonk'.
	
	filesystem createDirectory: directory.
	filesystem createDirectory: directory / 'alfa'.
	filesystem createDirectory: directory / 'beta'.
	
	self markForCleanup: directory / 'alfa'.
	self markForCleanup: directory / 'beta'.
	self markForCleanup: directory.
	
	self should: [sorted := (filesystem childrenAt: directory) sort] raise: Error.
"	self assert: sorted size = 2.
	self assert: (sorted at: 1) basename = 'alfa'.
	self assert: (sorted at: 2) basename = 'beta'."
]

{ #category : 'tests' }
ClientFileSystemTest >> testChildrenSortingRoot [
	| directory1 directory2 |
	"self skip."
	
	directory1 := Path * 'plonk1'.
	directory2 := Path * 'plonk2'.
	
	filesystem createDirectory: directory1.
	filesystem createDirectory: directory2.
	
	self markForCleanup: directory1.
	self markForCleanup: directory2.
	self should: [filesystem workingDirectory children] raise: Error
	"self assert: filesystem workingDirectory children sort size = filesystem workingDirectory children size"
]

{ #category : 'tests' }
ClientFileSystemTest >> testCopyWithCorrectBasename [
        | directory |
        self
                markForCleanup: 'gooly';
                markForCleanup: 'plonk'.
        directory := filesystem workingDirectory.
        (directory / 'gooly') ensureCreateFile.
        directory / 'gooly' copyTo: directory / 'plonk'.
        self assert: (directory / 'plonk') exists.
		"This test is not run because client does not support directory contents querries"
        "self assert: (directory childNames includes: 'plonk')"
]

{ #category : 'tests' }
ClientFileSystemTest >> testCreateDirectory [
	| path directory |
	directory := filesystem workingDirectory.
 	self markForCleanup: directory / 'plonk' / 'griffle'.
	self markForCleanup: directory / 'plonk'.
	path := directory / 'plonk' / 'griffle'.
	(directory / 'plonk') ensureCreateDirectory.
	self shouldnt: [path createDirectory] raise:Error.
	self assert: path exists.
	self should: [(directory / 'plonk' ) deleteAll] raise: Error description: 'GemStone does not support retrieval of client directory entries'
]

{ #category : 'tests' }
ClientFileSystemTest >> testCreateFile [
	| directory path |
	directory := filesystem workingDirectory.
 	self markForCleanup: directory / 'plonk' / 'griffles'.
	self markForCleanup: directory / 'plonk'.
	path := directory / 'plonk' / 'griffles'.
	(directory / 'plonk') ensureCreateDirectory.
	self shouldnt: [ path createFile] raise:Error.
	self assert:path exists .
	self should: [(directory / 'plonk' ) deleteAll] raise: Error description: 'GemStone does not support retrieval of client directory entries'
]

{ #category : 'tests' }
ClientFileSystemTest >> testCreation [

	| aClientStore |
	aClientStore := ClientStore new.
	filesystem := FileSystem store: aClientStore.
	self assert: aClientStore store class equals: ClientStore activeClass.
	self assert: aClientStore isForeignFileSystem.
	self assert: aClientStore defaultWorkingDirectory notNil.
	self assert: (aClientStore defaultWorkingDirectory isKindOf: Path).
	self assert: filesystem store defaultWorkingDirectory equals: aClientStore defaultWorkingDirectory.
]

{ #category : 'tests' }
ClientFileSystemTest >> testDefaultWorkingDirectory [
	| aFileReference |
	aFileReference := filesystem workingDirectory.

	ClientStore activeClass = WindowsStore ifTrue: [
		self assert: (aFileReference path at: 1) = 'C:'.
		self assert: (aFileReference path at: 2) = 'Users'.
		self assert: aFileReference path size = 3.
		^self].

	ClientStore activeClass = UnixStore ifTrue: [
		self assert: (aFileReference path at: 1) = 'Users'.
		^self].

	self assert: false description: 'Something is wrong here - need to fix the test'
]

{ #category : 'tests' }
ClientFileSystemTest >> testDelimiter [
	super testDelimiter.
	self assert: filesystem store delimiter = ClientStore activeClass delimiter
]

{ #category : 'tests' }
ClientFileSystemTest >> testEntriesAt [
	| directory entries |
	directory := Path * 'plonk'.
	
	self 
		markForCleanup: directory / 'griffle';
		markForCleanup: directory / 'bint';
		markForCleanup: directory.
	self cleanupPaths.

	filesystem createDirectory: directory.
	filesystem createDirectory: directory / 'griffle'.
	filesystem createDirectory: directory / 'bint'.
	
	self should: [entries := filesystem entriesAt: directory] raise: Error description: 'GemStone client does not support requests for directory entries'.
"	self assert: entries size = 2.
	entries do: [ :ea | 
		self assert: (ea isKindOf: FileSystemDirectoryEntry).
		self assert: ea reference parent path = (filesystem resolve: directory).
		self assert: (#('griffle' 'bint' ) includes: ea reference basename).
		self assert: ea isDirectory ]"
]

{ #category : 'tests' }
ClientFileSystemTest >> testEntryAt [
	| path1 path2 entry1 entry2  |

	path1 := Path * 'plonk1'.
	path2 := Path * 'plonk2'.
	self markForCleanup: path1.
	self markForCleanup: path2.
	
	filesystem createDirectory: path1.
	self waitForSeconds: 1.
	filesystem createDirectory: path2.
	
	entry1 := filesystem entryAt: path1.
	entry2 := filesystem entryAt: path2.
	
	self assert: entry1 isDirectory.
	self assert: entry2 isDirectory.
	self assert: entry1 reference = (filesystem referenceTo: path1) asAbsolute.
	self assert: entry2 reference = (filesystem referenceTo: path2) asAbsolute.

"	The following tests are removed because the creationTime is a fantasy since we cannot retrieve the client file information.
	self assert: entry1 creationTime < entry2 creationTime.
	self assert: entry1 modificationTime < entry2 modificationTime."
]

{ #category : 'tests' }
ClientFileSystemTest >> testFileNames [
	| reference |
	#('test one' 'test with two' 'test-äöü' 'test.äöü') do: [ :each |
		reference := filesystem workingDirectory / each.
		self assert: reference basename = each.
		self deny: reference exists.
		reference
			writeStreamDo: [ :stream | stream nextPutAll: 'gooly' ]
			ifPresent: [ self fail ].
		[	self assert: reference exists.
			"self assert: (filesystem workingDirectory children
				anySatisfy: [ :ref | ref = reference ]) "]
		ensure: [ reference delete ] ]
]

{ #category : 'tests' }
ClientFileSystemTest >> testIsDiskFileSystem [
	self assert: filesystem isDiskFileSystem.
]
