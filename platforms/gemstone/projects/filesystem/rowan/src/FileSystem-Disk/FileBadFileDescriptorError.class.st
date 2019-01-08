"
Bad file descriptor (POSIX.1-2001).
"
Class {
	#name : 'FileBadFileDescriptorError',
	#superclass : 'FilePosixError',
	#category : 'FileSystem-Disk-Errors'
}

{ #category : 'accessing' }
FileBadFileDescriptorError class >> defaultErrorNumber [
	^10
]

{ #category : 'accessing' }
FileBadFileDescriptorError class >> errorDescription [
	^'Bad file descriptor'
]

{ #category : 'accessing' }
FileBadFileDescriptorError class >> posixName [
	^'EBADF'
]

{ #category : 'accessing' }
FileBadFileDescriptorError class >> windowsErrorNumber [
	^9
]
