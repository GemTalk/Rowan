"
I/O error.

The path argument names a STREAMS file and a hangup or error occurred during the open().
"
Class {
	#name : 'FileIOError',
	#superclass : 'FilePosixError',
	#category : 'FileSystem-Disk-Errors'
}

{ #category : 'accessing' }
FileIOError class >> defaultErrorNumber [
	^5
]

{ #category : 'accessing' }
FileIOError class >> errorDescription [
	^'I/O error'
]

{ #category : 'accessing' }
FileIOError class >> posixName [
	^'EIO'
]
