Class {
	#name : 'FileDeviceNotSameError',
	#superclass : 'FilePosixError',
	#category : 'FileSystem-Base-Errors'
}

{ #category : 'accessing' }
FileDeviceNotSameError class >> defaultErrorNumber [
	^18
]

{ #category : 'accessing' }
FileDeviceNotSameError class >> errorDescription [
	^'Invalid cross-device link'
]

{ #category : 'accessing' }
FileDeviceNotSameError class >> posixName [
	^'EXDEV'
]
