"
Write I/O operations on the file descriptor shall complete as defined by synchronized I/O file integrity completion.
"
Class {
	#name : 'FileReadSynchronousFlag',
	#superclass : 'FileOpeningFlag',
	#category : 'FileSystem-Base-FileOptions'
}

{ #category : 'accessing' }
FileReadSynchronousFlag class >> posixName [
	^'O_RSYNC'
]
