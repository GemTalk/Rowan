"
File access mode flags:

The subclasses implement the modes for opening a Posix compliant file.
These modes are mutually exclusive.

In C they can be retrieved using the fcntl() F_GETFL operation
"
Class {
	#name : 'FileOpenMode',
	#superclass : 'FileOption',
	#category : 'FileSystem-Disk-Options'
}

{ #category : 'convenience' }
FileOpenMode >> accessRule [
	self subclassResponsibility
]

{ #category : 'testing' }
FileOpenMode >> isReadable [
	^false
]

{ #category : 'testing' }
FileOpenMode >> isWritable [
	^false
]

{ #category : 'printing' }
FileOpenMode >> printOpenModeStringOn: aStream [

	aStream nextPutAll: self mode
]
