Class {
	#name : 'FileReadBinaryMode',
	#superclass : 'FileReadMode',
	#category : 'FileSystem-Disk-Base'
}

{ #category : 'accessing' }
FileReadBinaryMode >> mode [
	"Open binary for reading and writing.  The stream is positioned at the beginning of the file."
	^'rb'
]
