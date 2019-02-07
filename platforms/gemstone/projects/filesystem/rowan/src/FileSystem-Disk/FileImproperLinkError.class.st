"
Improper link
"
Class {
	#name : 'FileImproperLinkError',
	#superclass : 'FilePosixError',
	#category : 'FileSystem-Disk-Errors'
}

{ #category : 'accessing' }
FileImproperLinkError class >> defaultErrorNumber [
	^18
]

{ #category : 'accessing' }
FileImproperLinkError class >> errorDescription [
	^'Invalid cross-device link'
]

{ #category : 'accessing' }
FileImproperLinkError class >> posixName [
	^'EXDEV'
]
