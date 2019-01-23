"
Write I/O operations on the file descriptor shall complete as defined by synchronized I/O data integrity completion.

If both the O_SYNC and O_DSYNC flags are set, the effect is as if only the O_SYNC flag was set.
"
Class {
	#name : 'FileSychronizedDataFlag',
	#superclass : 'FileOpeningFlag',
	#category : 'FileSystem-Disk-Options'
}

{ #category : 'accessing' }
FileSychronizedDataFlag class >> posixName [
	^'O_DSYNC'
]
