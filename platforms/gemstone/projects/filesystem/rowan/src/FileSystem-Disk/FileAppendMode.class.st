Class {
	#name : 'FileAppendMode',
	#superclass : 'FileMode',
	#category : 'FileSystem-Disk-Base'
}

{ #category : 'testing' }
FileAppendMode >> isWritable [
	^true
]

{ #category : 'accessing' }
FileAppendMode >> mode [
	"Open for appending (writing at end of file).  The file is
     created if it does not exist.  The stream is positioned at the
     end of the file."
	^'a'
]
