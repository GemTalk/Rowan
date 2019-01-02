"
Open the file for synchronous I/O.
"
Class {
	#name : 'FileSynchronousOption',
	#superclass : 'FileStatusOption',
	#category : 'FileSystem-Disk-Options'
}

{ #category : 'accessing' }
FileSynchronousOption class >> posixName [
	^'O_SYNC'
]

{ #category : 'accessing' }
FileSynchronousOption class >> posixValue [
	^self error: 'Not implmented'
]
