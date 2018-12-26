"
Device or resource busy
"
Class {
	#name : 'FileBusyError',
	#superclass : 'FilePosixError',
	#category : 'FileSystem-Disk-Errors'
}

{ #category : 'accessing' }
FileBusyError class >> errorNumber [
	^12
]

{ #category : 'accessing' }
FileBusyError class >> posixName [
	^'EBUSY'
]
