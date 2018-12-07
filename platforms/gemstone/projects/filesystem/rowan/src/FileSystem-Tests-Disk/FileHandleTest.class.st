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
