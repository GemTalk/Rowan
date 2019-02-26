Class {
	#name : 'FileSynchronizedIONotSupportedError',
	#superclass : 'FilePosixError',
	#category : 'FileSystem-Disk-Errors'
}

{ #category : 'accessing' }
FileSynchronizedIONotSupportedError class >> defaultErrorNumber [
	^999 "This needs to be determined"
]

{ #category : 'accessing' }
FileSynchronizedIONotSupportedError class >> errorDescription [
	^'Synchronized IO not supported'
]

{ #category : 'accessing' }
FileSynchronizedIONotSupportedError class >> posixName [
	^'EINTR'
]
