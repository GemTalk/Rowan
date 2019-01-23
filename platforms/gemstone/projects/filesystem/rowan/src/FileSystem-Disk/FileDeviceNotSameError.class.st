Class {
	#name : 'FileDeviceNotSameError',
	#superclass : 'FilePosixError',
	#category : 'FileSystem-Disk-Errors'
}

{ #category : 'accessing' }
FileDeviceNotSameError class >> defaultErrorNumber [
	^14
]

{ #category : 'accessing' }
FileDeviceNotSameError class >> errorDescription [
	^'Not same device'
]

{ #category : 'accessing' }
FileDeviceNotSameError class >> posixName [
	^'EBUSY'
]
