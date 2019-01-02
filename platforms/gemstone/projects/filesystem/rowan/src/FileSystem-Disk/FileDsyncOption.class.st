"
Perform file writes according to the requirements of synchronized I/O data integrity completion.
"
Class {
	#name : 'FileDsyncOption',
	#superclass : 'FileStatusOption',
	#category : 'FileSystem-Disk-Options'
}

{ #category : 'accessing' }
FileDsyncOption class >> posixName [
	^'O_DSYNC'
]

{ #category : 'accessing' }
FileDsyncOption class >> posixValue [
	^self error: 'Not implmented'
]
