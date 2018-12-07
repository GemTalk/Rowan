Class {
	#name : 'FileAppendBinaryMode',
	#superclass : 'FileAppendMode',
	#category : 'FileSystem-Disk-Base'
}

{ #category : 'accessing' }
FileAppendBinaryMode >> mode [
	"Open for appending (writing at end of file).  The file is
     created if it does not exist.  The stream is positioned at the
     end of the file."
	^'ab'
]
