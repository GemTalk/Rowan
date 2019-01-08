"
Used to indicate a shared (read) ADVISORY lock when the file is opened.

Linux supports this type of lock.
"
Class {
	#name : 'FileReadLockOption',
	#superclass : 'FileLockOption',
	#category : 'FileSystem-Disk-Options'
}

{ #category : 'accessing' }
FileReadLockOption class >> defaultPlatformValue [
	^1
]

{ #category : 'accessing' }
FileReadLockOption class >> posixName [
	^'F_RDLCK'
]
