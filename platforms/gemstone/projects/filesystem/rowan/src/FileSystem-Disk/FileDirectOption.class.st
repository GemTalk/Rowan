"
Allow file I/O to bypass the buffer cache. The _GNU_SOURCE feature test macro must be defined in order to make this constant definition available from <fcntl.h>.
"
Class {
	#name : 'FileDirectOption',
	#superclass : 'FileCreationOption',
	#category : 'FileSystem-Disk-Options'
}

{ #category : 'accessing' }
FileDirectOption class >> posixName [
	^'O_DIRECT'
]

{ #category : 'accessing' }
FileDirectOption class >> posixValue [
	self error: 'Not supported'
]
