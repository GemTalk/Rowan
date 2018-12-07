Class {
	#name : 'FileAppendAndReadBinaryMode',
	#superclass : 'FileAppendAndReadMode',
	#category : 'FileSystem-Disk'
}

{ #category : 'accessing' }
FileAppendAndReadBinaryMode >> mode [
	"Open for reading and appending (writing at end of file).  The
     file is created if it does not exist.  The initial file
     position for reading is at the beginning of the file, but
     output is always appended to the end of the file."
	^'a+b'
]
