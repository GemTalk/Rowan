"
SEEK_CUR  It moves file pointer position to given location.
"
Class {
	#name : 'FileSeekCurrentPosition',
	#superclass : 'FilePositioning',
	#category : 'FileSystem-Disk-Options'
}

{ #category : 'accessing' }
FileSeekCurrentPosition class >> posixName [
	^'SEEK_CUR'
]

{ #category : 'accessing' }
FileSeekCurrentPosition class >> posixValue [
	^1
]
