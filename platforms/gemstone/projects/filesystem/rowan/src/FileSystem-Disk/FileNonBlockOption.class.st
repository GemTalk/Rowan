"
Open the file in nonblocking mode.
"
Class {
	#name : 'FileNonBlockOption',
	#superclass : 'FileStatusOption',
	#category : 'FileSystem-Disk-Options'
}

{ #category : 'accessing' }
FileNonBlockOption class >> defaultPlatformValue [
	^16r00080
]

{ #category : 'accessing' }
FileNonBlockOption class >> posixName [
	^'O_NONBLOCK'
]
