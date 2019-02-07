"
The directory or file system that would contain the new file cannot be expanded, the file does not exist, and O_CREAT is specified
"
Class {
	#name : 'FileNoSpaceError',
	#superclass : 'FilePosixError',
	#category : 'FileSystem-Disk-Errors'
}

{ #category : 'accessing' }
FileNoSpaceError class >> defaultErrorNumber [
	^28
]

{ #category : 'accessing' }
FileNoSpaceError class >> errorDescription [
	^'No space left on device'
]

{ #category : 'accessing' }
FileNoSpaceError class >> posixName [
	^'ENOSPC'
]
