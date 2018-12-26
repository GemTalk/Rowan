"
More than {SYMLOOP_MAX} symbolic links were encountered during resolution of the path argument.
"
Class {
	#name : 'FileTooManySymbolicLinksError',
	#superclass : 'FilePosixError',
	#category : 'FileSystem-Disk-Errors'
}

{ #category : 'accessing' }
FileTooManySymbolicLinksError class >> errorNumber [
	^10062
]

{ #category : 'accessing' }
FileTooManySymbolicLinksError class >> posixName [
	^'ELOOP'
]
