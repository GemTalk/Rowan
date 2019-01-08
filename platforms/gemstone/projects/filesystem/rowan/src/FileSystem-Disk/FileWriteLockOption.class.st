"
Used to indicate a exclusive (write) ADVISORY lock when the file is opened.

Linux supports this type of lock.
"
Class {
	#name : 'FileWriteLockOption',
	#superclass : 'FileLockOption',
	#category : 'FileSystem-Disk-Options'
}

{ #category : 'accessing' }
FileWriteLockOption class >> defaultPlatformValue [
	^2
]

{ #category : 'accessing' }
FileWriteLockOption class >> posixName [
	^'F_WRLCK'
]
