"
SUnit tests for FileSystemHandle
"
Class {
	#name : 'FileSystemHandleTest',
	#superclass : 'TestCase',
	#instVars : [
		'filesystem',
		'handle',
		'reference'
	],
	#category : 'FileSystem-Tests-Core-Base'
}

{ #category : 'testing' }
FileSystemHandleTest class >> isAbstract [
	^ self name = #FileSystemHandleTest
]

{ #category : 'testing' }
FileSystemHandleTest class >> shouldInheritSelectors [ 
	^ true
]

{ #category : 'running' }
FileSystemHandleTest >> createFileSystem [
	self subclassResponsibility 
]

{ #category : 'running' }
FileSystemHandleTest >> reopenForRead [
]

{ #category : 'running' }
FileSystemHandleTest >> setUp [
	super setUp.
	filesystem := self createFileSystem.
	reference := filesystem * 'plonk'.
	handle := reference openOptions: FileOptions newWrite
]

{ #category : 'running' }
FileSystemHandleTest >> tearDown [
	handle ensureClosed.
	reference ensureDelete.
	super tearDown
]

{ #category : 'tests' }
FileSystemHandleTest >> testAt [
	handle at: 1 write: (ByteArray with: 3) startingAt: 1 count: 1.
	self reopenForRead.
	self assert: (handle at: 1) = 3
]

{ #category : 'tests' }
FileSystemHandleTest >> testAtPut [
	| in |
	handle at: 1 put: 3.
	in := ByteArray new: 1.
	self reopenForRead.
	handle at: 1 read: in startingAt: 1 count: 1.
	self assert: in first = 3
]

{ #category : 'tests' }
FileSystemHandleTest >> testAtPutBinaryAscii [
	handle at: 1 put: 32.
	handle at: 1 put: Character space
]

{ #category : 'tests' }
FileSystemHandleTest >> testAtWriteBinaryAscii [
	handle
		at: 1
		write: #[32]
		startingAt: 1
		count: 1.
	handle
		at: 1
		write: (String with: Character space)
		startingAt: 1
		count: 1
]

{ #category : 'tests' }
FileSystemHandleTest >> testClose [
	handle close.
	self deny: handle isOpen
	
]

{ #category : 'tests' }
FileSystemHandleTest >> testCreatedOpen [
	
	
	self flag: 'TODO: activated once FileHandle is in use again!'.
	"self assert: handle isOpen"
]

{ #category : 'tests' }
FileSystemHandleTest >> testEnsureClosed [

	filesystem := self createFileSystem.
	reference := filesystem * 'plonk'.
	handle := reference openOptions: FileOptions newWrite.
	handle ensureClosed.
	self deny: handle isOpen.
	handle ensureClosed.
	reference ensureDelete.
	handle reference exists
		ifTrue: [ self error ].
	handle ensureClosed
]

{ #category : 'tests' }
FileSystemHandleTest >> testIO [
	| out in |
	out := #(1 2 3) asByteArray.
	in := ByteArray new: 3.
	handle at: 1 write: out startingAt: 1 count: 3.
	self reopenForRead.
	handle at: 1 read: in startingAt: 1 count: 3.
	self assert: out = in.
]

{ #category : 'tests' }
FileSystemHandleTest >> testOpenOptions [
	| aFileOptions |
	aFileOptions := FileOptions newRead.
	handle := reference openOptions: aFileOptions.
	self assert: (handle isKindOf: self targetClass).
	self assert: handle options == aFileOptions
]

{ #category : 'tests' }
FileSystemHandleTest >> testReadBufferTooLarge [
	| out in result |
	out := #(1 2 3) asByteArray.
	in := ByteArray new: 5.
	in atAllPut: 9.
	handle at: 1 write: out startingAt: 1 count: 3.
	self reopenForRead.
	result := handle at: 1 read: in startingAt: 2 count: 4.
	self assert: result = 3.
	self assert: in = #(9 1 2 3 9) asByteArray.
]

{ #category : 'tests' }
FileSystemHandleTest >> testReadOnly [
	handle close.
	handle := reference openOptions: FileOptions newRead.
	self 
		should: 
			[ handle 
				at: 1
				write: #(1 2 3 )
				startingAt: 1
				count: 3 ]
		raise: Error
]

{ #category : 'tests' }
FileSystemHandleTest >> testReference [
	self assert: handle reference = reference asAbsolute
]

{ #category : 'tests' }
FileSystemHandleTest >> testSizeAfterGrow [
	| out |
	out := #(1 2 3) asByteArray.
	handle at: 1 write: out startingAt: 1 count: 3.
	self assert: handle size = 3
]

{ #category : 'tests' }
FileSystemHandleTest >> testSizeNoGrow [
	| bytes |
	bytes := #(1 2 3 4) asByteArray.
	handle at: 1 write: bytes startingAt: 1 count: 3.
	handle at: 4 write: bytes startingAt: 4 count: 1.
	self assert: handle size = 4
]

{ #category : 'tests' }
FileSystemHandleTest >> testTruncate [
	| out |
	out := #(1 2 3 4 5) asByteArray.
	handle at: 1 write: out startingAt: 1 count: 5.
	self assert: handle size = 5.
	handle truncateTo: 3.
	"The Disk version will fail until the primitive is implemented"
	self assert: handle size = 3
]
