Class {
	#name : 'FileReadOnlyFileSystemError',
	#superclass : 'FilePosixError',
	#category : 'FileSystem-Base-Errors'
}

{ #category : 'accessing' }
FileReadOnlyFileSystemError class >> defaultErrorNumber [
	^19
]

{ #category : 'accessing' }
FileReadOnlyFileSystemError class >> errorDescription [
	^'Read only file system'
]

{ #category : 'accessing' }
FileReadOnlyFileSystemError class >> posixName [
	^'EROFS'
]
