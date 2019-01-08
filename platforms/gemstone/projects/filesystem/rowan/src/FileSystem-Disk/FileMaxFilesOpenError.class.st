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
	^17
]

{ #category : 'other' }
FileMaxFilesOpenError class >> errorDescription [
	^'Too many open files'
]

{ #category : 'accessing' }
FileMaxFilesOpenError class >> posixName [
	^'ENFILE'
]

{ #category : 'accessing' }
FileMaxFilesOpenError class >> windowsErrorNumber [
	^23
]
