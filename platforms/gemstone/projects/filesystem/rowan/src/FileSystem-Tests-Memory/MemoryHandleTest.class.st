"
SUnit tests for memory handles, the tests may be found in superclass
"
Class {
	#name : 'MemoryHandleTest',
	#superclass : 'FileSystemHandleTest',
	#category : 'FileSystem-Tests-Memory-Base'
}

{ #category : 'running' }
MemoryHandleTest >> createFileSystem [
	^ FileSystem memory
]

{ #category : 'tests' }
MemoryHandleTest >> testTruncate [
	| out |
	out := #(1 2 3 4 5) asByteArray.
	handle at: 1 write: out startingAt: 1 count: 5.
	self assert: handle size = 5.
	handle truncateTo: 3.
	"The Disk version will fail until the primitive is implemented"
	self assert: handle size = 3
]

{ #category : 'tests' }
MemoryHandleTest >> testWriteStream [
	| stream |
	stream := handle binaryStream.
	self assert: (stream isKindOf: MemoryFileWriteStream).
	self assert: (stream respondsTo: #nextPut:)
]
