"
Bad file descriptor (POSIX.1-2001).
"
Class {
	#name : 'FileBadFileDescriptorError',
	#superclass : 'FilePosixError',
	#category : 'FileSystem-Disk-Errors'
}

{ #category : 'accessing' }
FileBadFileDescriptorError class >> errorNumber [
	^10
]

{ #category : 'accessing' }
FileBadFileDescriptorError class >> posixName [
	^'EBADF'
]
