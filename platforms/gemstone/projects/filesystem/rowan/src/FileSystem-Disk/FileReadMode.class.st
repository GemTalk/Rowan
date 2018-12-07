Class {
	#name : 'FileReadMode',
	#superclass : 'FileMode',
	#category : 'FileSystem-Disk-Base'
}

{ #category : 'testing' }
FileReadMode >> isWritable [
	^false
]

{ #category : 'accessing' }
FileReadMode >> mode [
	"Open for reading and writing.  The stream is positioned at the beginning of the file."
	^'r'
]
