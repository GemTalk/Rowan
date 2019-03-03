"
close-on-exec flag
"
Class {
	#name : 'FileCloseOnExecutionFlag',
	#superclass : 'FileControlFlag',
	#category : 'FileSystem-Base-FileOptions'
}

{ #category : 'access' }
FileCloseOnExecutionFlag class >> defaultPlatformValue [
	^1
]

{ #category : 'access' }
FileCloseOnExecutionFlag class >> posixName [
	^'FD_CLOEXEC'
]
