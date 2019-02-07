"
The maximum allowable number of files is currently open in the system
"
Class {
	#name : 'FileMaxFilesOpenError',
	#superclass : 'FilePosixError',
	#category : 'FileSystem-Disk-Errors'
}

{ #category : 'accessing' }
FileMaxFilesOpenError class >> defaultErrorNumber [
	^23
]

{ #category : 'accessing' }
FileMaxFilesOpenError class >> errorDescription [
	^'TToo many open files in system'
]

{ #category : 'accessing' }
FileMaxFilesOpenError class >> posixName [
	^'ENFILE'
]
