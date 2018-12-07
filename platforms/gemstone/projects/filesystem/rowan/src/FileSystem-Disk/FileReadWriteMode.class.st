Class {
	#name : 'FileReadWriteMode',
	#superclass : 'FileMode',
	#category : 'FileSystem-Disk-Base'
}

{ #category : 'testing' }
FileReadWriteMode >> isWritable [
	^true
]

{ #category : 'accessing' }
FileReadWriteMode >> mode [
	"Open for reading and writing.  The stream is positioned at the beginning of the file. If the file does not exist, nil is returned"
	^'r+'
]
