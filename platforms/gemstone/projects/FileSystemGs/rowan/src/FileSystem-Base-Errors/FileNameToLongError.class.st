"
The length of the path argument exceeds {PATH_MAX} or a pathname component is longer than {NAME_MAX}.

This is used with sockets.
"
Class {
	#name : 'FileNameToLongError',
	#superclass : 'FilePosixError',
	#category : 'FileSystem-Base-Errors'
}

{ #category : 'accessing' }
FileNameToLongError class >> defaultErrorNumber [
	^36
]

{ #category : 'accessing' }
FileNameToLongError class >> errorDescription [
	^'Name is too long'
]

{ #category : 'accessing' }
FileNameToLongError class >> posixName [
	^'ENAMETOOLONG'
]
