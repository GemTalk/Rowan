"
Used to indicate a shared (read) ADVISORY lock when the file is opened.

Linux supports this type of lock.
"
Class {
	#name : 'FileReadLockFlag',
	#superclass : 'FileLockFlag',
	#category : 'FileSystem-Base-FileOptions'
}

{ #category : 'accessing' }
FileReadLockFlag class >> defaultPlatformValue [
	^1
]

{ #category : 'accessing' }
FileReadLockFlag class >> posixName [
	^'F_RDLCK'
]
