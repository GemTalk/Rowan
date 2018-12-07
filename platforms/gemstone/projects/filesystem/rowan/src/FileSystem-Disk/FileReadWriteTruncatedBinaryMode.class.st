Class {
	#name : 'FileReadWriteTruncatedBinaryMode',
	#superclass : 'FileReadWriteTruncatedMode',
	#category : 'FileSystem-Disk-Base'
}

{ #category : 'accessing' }
FileReadWriteTruncatedBinaryMode >> mode [
	"Open binary file for reading and writing.  The file is created if it does
      not exist, otherwise it is truncated.  The stream is
      positioned at the beginning of the file."
	^'w+b'
]
