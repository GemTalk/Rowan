"
More than {SYMLOOP_MAX} symbolic links were encountered during resolution of the path argument.
"
Class {
	#name : 'FileTooManySymbolicLinksError',
	#superclass : 'FilePosixError',
	#category : 'FileSystem-Base-Errors'
}

{ #category : 'accessing' }
FileTooManySymbolicLinksError class >> defaultErrorNumber [
	^40
]

{ #category : 'accessing' }
FileTooManySymbolicLinksError class >> errorDescription [
	^'Too many levels of symbolic links'
]

{ #category : 'accessing' }
FileTooManySymbolicLinksError class >> posixName [
	^'ELOOP'
]
