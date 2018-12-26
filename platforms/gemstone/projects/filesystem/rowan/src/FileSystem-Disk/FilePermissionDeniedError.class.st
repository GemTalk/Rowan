"
Operation not permitted error. Concerning capability to mount a file system.
"
Class {
	#name : 'FilePermissionDeniedError',
	#superclass : 'FilePosixError',
	#category : 'FileSystem-Disk-Errors'
}

{ #category : 'accessing' }
FilePermissionDeniedError class >> errorNumber [
	^7
]

{ #category : 'accessing' }
FilePermissionDeniedError class >> posixName [
	^'EPERM'
]
