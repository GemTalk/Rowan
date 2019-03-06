"
Bad file descriptor (POSIX.1-2001).
"
Class {
	#name : 'FileBadFileDescriptorError',
	#superclass : 'FilePosixError',
	#category : 'FileSystem-Base-Errors'
}

{ #category : 'accessing' }
FileBadFileDescriptorError class >> defaultErrorNumber [
	^9
]

{ #category : 'accessing' }
FileBadFileDescriptorError class >> errorDescription [
	^'Bad file descriptor'
]

{ #category : 'accessing' }
FileBadFileDescriptorError class >> posixName [
	^'EBADF'
]
