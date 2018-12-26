"
Writes are always appended to the end of the file.
"
Class {
	#name : 'FileAppendOption',
	#superclass : 'FileStatusOption',
	#category : 'FileSystem-Disk-Options'
}

{ #category : 'accessing' }
FileAppendOption class >> posixName [
	^'O_APPEND'
]

{ #category : 'accessing' }
FileAppendOption class >> posixValue [
	^16r10000
]
