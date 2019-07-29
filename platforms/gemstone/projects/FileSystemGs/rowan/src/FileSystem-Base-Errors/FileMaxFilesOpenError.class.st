"
The maximum allowable number of files is currently open in the system
"
Class {
	#name : 'FileMaxFilesOpenError',
	#superclass : 'FilePosixError',
	#category : 'FileSystem-Base-Errors'
}

{ #category : 'accessing' }
FileMaxFilesOpenError class >> defaultErrorNumber [
	^23
]

{ #category : 'accessing' }
FileMaxFilesOpenError class >> errorDescription [
	^'Too many open files'
]

{ #category : 'accessing' }
FileMaxFilesOpenError class >> posixName [
	^'ENFILE'
]
