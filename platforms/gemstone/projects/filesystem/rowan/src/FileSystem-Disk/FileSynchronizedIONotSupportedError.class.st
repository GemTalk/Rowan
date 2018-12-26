Class {
	#name : 'FileSynchronizedIONotSupportedError',
	#superclass : 'FilePosixError',
	#category : 'FileSystem-Disk-Errors'
}

{ #category : 'accessing' }
FileSynchronizedIONotSupportedError class >> errorNumber [
	self error: 'This needs to be defined'
]

{ #category : 'accessing' }
FileSynchronizedIONotSupportedError class >> posixName [
	^'EINTR'
]
