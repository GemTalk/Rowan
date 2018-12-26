Class {
	#name : 'FileReadOnlyFileSystemError',
	#superclass : 'FilePosixError',
	#category : 'FileSystem-Disk-Errors'
}

{ #category : 'accessing' }
FileReadOnlyFileSystemError class >> errorNumber [
	^19
]

{ #category : 'accessing' }
FileReadOnlyFileSystemError class >> posixName [
	^'EROFS'
]
