"
SEEK_CUR  It moves file pointer position to given location.
"
Class {
	#name : 'FileSeekCurrentPosition',
	#superclass : 'FilePositioning',
	#category : 'FileSystem-Base-FileOptions'
}

{ #category : 'accessing' }
FileSeekCurrentPosition class >> defaultPlatformValue [
	^1
]

{ #category : 'accessing' }
FileSeekCurrentPosition class >> posixName [
	^'SEEK_CUR'
]
