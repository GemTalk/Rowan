"
The named file is a directory and oflag includes O_WRONLY or O_RDWR
"
Class {
	#name : 'FileIsDirectoryError',
	#superclass : 'FilePosixError',
	#category : 'FileSystem-Base-Errors'
}

{ #category : 'accessing' }
FileIsDirectoryError class >> defaultErrorNumber [
	^21
]

{ #category : 'accessing' }
FileIsDirectoryError class >> errorDescription [
	^'Is a directory'
]

{ #category : 'accessing' }
FileIsDirectoryError class >> posixName [
	^'EISDIR'
]
