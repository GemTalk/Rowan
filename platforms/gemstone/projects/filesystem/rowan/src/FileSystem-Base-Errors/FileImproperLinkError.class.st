"
Improper link
"
Class {
	#name : 'FileImproperLinkError',
	#superclass : 'FilePosixError',
	#category : 'FileSystem-Base-Errors'
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
FileImproperLinkError class >> posixName [
	^'TBD'  "Lost the name - needs to be fixed"
]
