"
File access mode flags:

The subclasses implement the modes for opening a Posix compliant file.
These modes are mutually exclusive.

In C they can be retrieved using the fcntl() F_GETFL operation
"
Class {
	#name : 'FileOpenModeFlag',
	#superclass : 'FileOpeningFlag',
	#category : 'FileSystem-Base-FileOptions'
}

{ #category : 'convenience' }
FileOpenModeFlag >> accessRule [
	self subclassResponsibility
]

{ #category : 'testing' }
FileOpenModeFlag >> isReadable [
	^false
]

{ #category : 'testing' }
FileOpenModeFlag >> isWritable [
	^false
]

{ #category : 'printing' }
FileOpenModeFlag >> printOpenModeStringOn: aStream [

	aStream nextPutAll: self mode
]
