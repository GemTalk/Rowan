Class {
	#name : 'FileNonBlockFlag',
	#superclass : 'FileOpeningFlag',
	#category : 'FileSystem-Base-FileOptions'
}

{ #category : 'accessing' }
FileNonBlockFlag class >> defaultPlatformValue [
	^16r00080
]

{ #category : 'accessing' }
FileNonBlockFlag class >> posixName [
	^'O_NONBLOCK'
]
