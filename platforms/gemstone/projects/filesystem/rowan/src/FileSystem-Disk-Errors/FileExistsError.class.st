"
O_CREAT and O_EXCL are set, and the named file exists.
"
Class {
	#name : 'FileExistsError',
	#superclass : 'FilePosixError',
	#category : 'FileSystem-Disk-Errors'
}

{ #category : 'accessing' }
FileExistsError class >> defaultErrorNumber [
	^13
]

{ #category : 'accessing' }
FileExistsError class >> errorDescription [
	^'File exists'
]

{ #category : 'accessing' }
FileExistsError class >> posixName [
	^'EEXIST'
]
