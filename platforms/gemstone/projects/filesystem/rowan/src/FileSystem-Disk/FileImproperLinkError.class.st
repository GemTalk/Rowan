"
Improper link
"
Class {
	#name : 'FileImproperLinkError',
	#superclass : 'FilePosixError',
	#category : 'FileSystem-Disk-Errors'
}

{ #category : 'accessing' }
FileImproperLinkError class >> errorNumber [
	^14
]

{ #category : 'accessing' }
FileImproperLinkError class >> posixName [
	^'EXDEV'
]
