"
Operation not permitted error. Concerning capability to mount a file system.
"
Class {
	#name : 'FilePermissionDeniedError',
	#superclass : 'FilePosixError',
	#category : 'FileSystem-Base-Errors'
}

{ #category : 'accessing' }
FilePermissionDeniedError class >> defaultErrorNumber [
	^1
]

{ #category : 'accessing' }
FilePermissionDeniedError class >> errorDescription [
	^'Permission denied or not owner'
]

{ #category : 'accessing' }
FilePermissionDeniedError class >> posixName [
	^'EPERM'
]
