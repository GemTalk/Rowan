"
Used to indicate a exclusive (write) ADVISORY lock when the file is opened.

Linux supports this type of lock.
"
Class {
	#name : 'FileWriteLockFlag',
	#superclass : 'FileLockFlag',
	#category : 'FileSystem-Base-FileOptions'
}

{ #category : 'accessing' }
FileWriteLockFlag class >> defaultPlatformValue [
	^2
]

{ #category : 'accessing' }
FileWriteLockFlag class >> posixName [
	^'F_WRLCK'
]
