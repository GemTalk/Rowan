"
The maximum allowable number of files is currently open in the system
"
Class {
	#name : 'FileMaxFilesOpenError',
	#superclass : 'FilePosixError',
	#category : 'FileSystem-Disk-Errors'
}

{ #category : 'accessing' }
FileMaxFilesOpenError class >> errorNumber [
	^17
]

{ #category : 'accessing' }
FileMaxFilesOpenError class >> posixName [
	^'EMFILE'
]
