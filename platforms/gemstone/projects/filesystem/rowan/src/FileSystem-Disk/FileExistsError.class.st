"
O_CREAT and O_EXCL are set, and the named file exists.
"
Class {
	#name : 'FileExistsError',
	#superclass : 'FilePosixError',
	#category : 'FileSystem-Disk-Errors'
}

{ #category : 'accessing' }
FileExistsError class >> errorDescription [
	^'File exists. An existing file was mentioned in an inappropriate context; for example, as a new link name in the link() function.'
]

{ #category : 'accessing' }
FileExistsError class >> errorFor: errorNumber path: path fileSystem: aFileSystem [
	(#(5 87) includes: errorNumber and: [aFileSystem privateIsDirectory: path]) ifTrue: [^true].
	(#(80) includes: errorNumber and: ["(oflag bitAnd: OEXCL) == 0"]) ifTrue: [^true].
	^false
]

{ #category : 'accessing' }
FileExistsError class >> errorNumber [
	^17
]

{ #category : 'accessing' }
FileExistsError class >> posixName [
	^'EEXIST'
]
