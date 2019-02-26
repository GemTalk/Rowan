"
SEEK_SET  It moves file pointer position to the beginning of the file.
"
Class {
	#name : 'FileSeekSetPosition',
	#superclass : 'FilePositioning',
	#category : 'FileSystem-Disk-Options'
}

{ #category : 'accessing' }
FileSeekSetPosition class >> defaultPlatformValue [
	^0
]

{ #category : 'accessing' }
FileSeekSetPosition class >> posixName [
	^'SEEK_SET'
]
