Class {
	#name : 'FileReadWriteBinaryMode',
	#superclass : 'FileReadWriteMode',
	#category : 'FileSystem-Disk-Base'
}

{ #category : 'accessing' }
FileReadWriteBinaryMode >> mode [
	"Open binary file for reading and writing.  The stream is positioned at the beginning of the file."
	^'r+b'
]
