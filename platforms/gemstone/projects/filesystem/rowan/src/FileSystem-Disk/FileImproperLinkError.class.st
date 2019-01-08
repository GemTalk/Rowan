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
	^14
]

{ #category : 'accessing' }
FileImproperLinkError class >> errorDescription [
	^'Not same device'
]

{ #category : 'accessing' }
FileImproperLinkError class >> errorNumber [
	^14
]

{ #category : 'accessing' }
FileImproperLinkError class >> windowsErrorNumber [
	^18
]
