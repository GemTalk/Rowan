"
The value of an file option argument is not valid.
"
Class {
	#name : 'FileInvalidOptionError',
	#superclass : 'FilePosixError',
	#category : 'FileSystem-Disk-Errors'
}

{ #category : 'accessing' }
FileInvalidOptionError class >> errorNumber [
	^16
]

{ #category : 'accessing' }
FileInvalidOptionError class >> posixName [
	^'EINVAL'
]
