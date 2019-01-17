"
SUnit tests for file handles, the tests may be found in superclass
"
Class {
	#name : 'FileHandleTest',
	#superclass : 'FileSystemHandleTest',
	#category : 'FileSystem-Tests-Disk-Base'
}

{ #category : 'running' }
FileHandleTest >> createFileSystem [
	"Force the filesystem to be created for each test.
	 Normally use: 'FileSystem disk' which returns the cached file system"
	^ FileSystem store: DiskStore activeClass createDefault
]

{ #category : 'tests' }
FileHandleTest >> testFileWriteError [
	" This test may be not needed depending on what happens with BinaryFileStream "
	| stream  |
	stream := FileSystem disk binaryWriteStreamOn: (FileSystem disk workingDirectory / 'foo.txt') path.
	stream close.
	self should: [stream  nextPutAll: 'abdef'] raise: FileWriteError
]

{ #category : 'tests' }
FileHandleTest >> testWriteStream [
	| stream |
	stream := handle binaryStream.
	self assert: (stream isKindOf: BinaryFileStream).
	self assert: (stream respondsTo: #nextPut:)
]
