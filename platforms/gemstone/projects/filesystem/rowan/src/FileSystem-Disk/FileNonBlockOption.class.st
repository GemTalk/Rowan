"
Open the file in nonblocking mode.
"
Class {
	#name : 'FileNonBlockOption',
	#superclass : 'FileStatusOption',
	#category : 'FileSystem-Disk-Options'
}

{ #category : 'accessing' }
FileNonBlockOption class >> posixName [
	^'O_NONBLOCK'
]

{ #category : 'accessing' }
FileNonBlockOption class >> posixValue [
	^16r00080
]
