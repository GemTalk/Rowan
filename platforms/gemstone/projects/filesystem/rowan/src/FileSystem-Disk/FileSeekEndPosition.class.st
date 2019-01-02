"
SEEK_END  It moves file pointer position to the end of file.
"
Class {
	#name : 'FileSeekEndPosition',
	#superclass : 'FilePositioning',
	#category : 'FileSystem-Disk-Options'
}

{ #category : 'accessing' }
FileSeekEndPosition class >> posixName [
	^'SEEK_END'
]

{ #category : 'accessing' }
FileSeekEndPosition class >> posixValue [
	^2
]
