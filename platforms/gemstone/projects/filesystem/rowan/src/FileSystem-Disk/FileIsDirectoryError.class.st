"
The named file is a directory and oflag includes O_WRONLY or O_RDWR
"
Class {
	#name : 'FileIsDirectoryError',
	#superclass : 'FilePosixError',
	#category : 'FileSystem-Disk-Errors'
}

{ #category : 'accessing' }
FileIsDirectoryError class >> defaultErrorNumber [
	^15
]

{ #category : 'accessing' }
FileIsDirectoryError class >> errorDescription [
	^'Is a directory'
]

{ #category : 'accessing' }
FileIsDirectoryError class >> posixName [
	^'EISDIR'
]

{ #category : 'accessing' }
FileIsDirectoryError class >> windowsErrorNumber [
	^21
]
