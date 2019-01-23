"
close-on-exec flag
"
Class {
	#name : 'FileCloseOnExecutionFlag',
	#superclass : 'FileControlFlag',
	#category : 'FileSystem-Disk-Options'
}

{ #category : 'access' }
FileCloseOnExecutionFlag class >> defaultPlatformValue [
	^1
]

{ #category : 'access' }
FileCloseOnExecutionFlag class >> posixName [
	^'FD_CLOEXEC'
]
