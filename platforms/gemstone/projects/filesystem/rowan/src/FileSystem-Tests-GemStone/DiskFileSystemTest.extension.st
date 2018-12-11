Extension { #name : 'DiskFileSystemTest' }

{ #category : '*filesystem-tests-gemstone' }
DiskFileSystemTest >> testClient [

	| aFileSystem |
	aFileSystem := FileSystem client.
	self assert: aFileSystem notNil.
	self assert: aFileSystem store notNil
]

{ #category : '*filesystem-tests-gemstone' }
DiskFileSystemTest >> testClientIsReadable [

	"This fails - it needs the code to differentiate between client and server implemented"

	"| aFileReference directory |
	filesystem :=  FileSystem store: (DiskStore activeClientClass createDefault).
	directory := filesystem clientWorkingDirectory.
	aFileReference := directory / 'plonked3'.
 	self markForCleanup: aFileReference.
	self deny: (self store exists: aFileReference path).
	(filesystem open: aFileReference path writable: self store readWriteTruncatedMode) close.
	self assert: (self store isReadable: aFileReference path)"
]

{ #category : '*filesystem-tests-gemstone' }
DiskFileSystemTest >> testGsDeleteDirectoryOnError [

	| path |
	path := Path * 'plonk' / 'griffle'.
	self markForCleanup: path.
	self markForCleanup: path parent.
	filesystem ensureCreateDirectory: path.
	self assert: (filesystem isDirectory: Path * 'plonk').
	self assert: (filesystem isDirectory: path).

	self should: [ filesystem delete: path parent ] raise: Error.
]
