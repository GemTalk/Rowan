Class {
	#name : 'FileAppendAndReadMode',
	#superclass : 'FileAppendMode',
	#category : 'FileSystem-Disk-Base'
}

{ #category : 'accessing' }
FileAppendAndReadMode >> mode [
	"Open for reading and appending (writing at end of file).  The
     file is created if it does not exist.  The initial file
     position for reading is at the beginning of the file, but
     output is always appended to the end of the file."
	^'a+'
]
