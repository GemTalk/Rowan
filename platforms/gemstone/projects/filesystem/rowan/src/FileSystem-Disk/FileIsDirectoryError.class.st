"
The named file is a directory and oflag includes O_WRONLY or O_RDWR
"
Class {
	#name : 'FileIsDirectoryError',
	#superclass : 'FilePosixError',
	#category : 'FileSystem-Disk-Errors'
}

{ #category : 'accessing' }
FileIsDirectoryError class >> errorNumber [
	^21
]

{ #category : 'accessing' }
FileIsDirectoryError class >> posixName [
	^'EISDIR'
]
