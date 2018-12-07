Class {
	#name : 'FileReadWriteTruncatedMode',
	#superclass : 'FileReadWriteMode',
	#category : 'FileSystem-Disk-Base'
}

{ #category : 'accessing' }
FileReadWriteTruncatedMode >> mode [
	"Open for reading and writing.  The file is created if it does
      not exist, otherwise it is truncated.  The stream is
      positioned at the beginning of the file."
	^'w+'
]
