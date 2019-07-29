"
Write I/O operations on the file descriptor shall complete as defined by synchronized I/O file integrity completion.

If both the O_SYNC and O_DSYNC flags are set, the effect is as if only the O_SYNC flag was set.
"
Class {
	#name : 'FileSynchronizedIOFlag',
	#superclass : 'FileOpeningFlag',
	#category : 'FileSystem-Base-FileOptions'
}

{ #category : 'accessing' }
FileSynchronizedIOFlag class >> posixName [
	^'O_SYNC'
]
