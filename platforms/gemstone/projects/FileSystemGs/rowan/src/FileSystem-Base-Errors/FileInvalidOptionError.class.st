"
The value of an file option argument is not valid.
"
Class {
	#name : 'FileInvalidOptionError',
	#superclass : 'FilePosixError',
	#category : 'FileSystem-Base-Errors'
}

{ #category : 'accessing' }
FileInvalidOptionError class >> defaultErrorNumber [
	^22
]

{ #category : 'accessing' }
FileInvalidOptionError class >> errorDescription [
	^'The function is being used incorrectly.  An argument may be invalid.'
]

{ #category : 'accessing' }
FileInvalidOptionError class >> posixName [
	^'EINVAL'
]
