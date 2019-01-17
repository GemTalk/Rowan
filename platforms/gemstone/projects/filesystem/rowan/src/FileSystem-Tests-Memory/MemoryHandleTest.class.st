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
MemoryHandleTest >> testWriteStream [
	| stream |
	stream := handle binaryStream.
	self assert: (stream isKindOf: MemoryFileWriteStream).
	self assert: (stream respondsTo: #nextPut:)
]
