"
Device or resource busy
"
Class {
	#name : 'FileBusyError',
	#superclass : 'FilePosixError',
	#category : 'FileSystem-Base-Errors'
}

{ #category : 'accessing' }
FileBusyError class >> defaultErrorNumber [
	^16
]

{ #category : 'accessing' }
FileBusyError class >> errorDescription [
	^'Devince or resource busy'
]

{ #category : 'accessing' }
FileBusyError class >> posixName [
	^'EBUSY'
]
