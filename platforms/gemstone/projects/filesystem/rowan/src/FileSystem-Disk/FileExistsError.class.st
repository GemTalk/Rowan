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
FileExistsError class >> errorFor: errorNumber path: path fileSystem: aFileSystem [
	(#(5 87) includes: errorNumber and: [aFileSystem privateIsDirectory: path]) ifTrue: [^true].
	(#(80) includes: errorNumber and: ["(oflag bitAnd: OEXCL) == 0"]) ifTrue: [^true].
	^false
]

{ #category : 'accessing' }
FileExistsError class >> posixName [
	^'EEXIST'
]

{ #category : 'accessing' }
FileExistsError class >> windowsErrorNumber [
	^17
]
