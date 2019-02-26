"
Search permission is denied on a component of the path prefix, or the file exists and the permissions specified by oflag are denied, or the file does not exist and write permission is denied for the parent directory of the file to be created, or O_TRUNC is specified and write permission is denied.
"
Class {
	#name : 'FileAccessError',
	#superclass : 'FilePosixError',
	#category : 'FileSystem-Disk-Errors'
}

{ #category : 'access' }
FileAccessError class >> defaultErrorNumber [
	^11
]

{ #category : 'access' }
FileAccessError class >> errorDescription [
	^'Invalid path or access denied'
]

{ #category : 'access' }
FileAccessError class >> posixName [
	^'EACCES'
]
