"
Acquire a lock (when l_type is F_RDLCK or F_WRLCK) or release a lock (when l_type is F_UNLCK) on the bytes specified by the l_whence, l_start, and l_len fields of lock. If a conflicting lock is held by another process, this call returns -1 and sets errno to EACCES or EAGAIN.
"
Class {
	#name : 'FileSetLockFlag',
	#superclass : 'FileControlFlag',
	#category : 'FileSystem-Base-FileOptions'
}

{ #category : 'accessing' }
FileSetLockFlag class >> defaultPlatformValue [
	^6
]

{ #category : 'accessing' }
FileSetLockFlag class >> posixName [
	^'F_SETLK'
]
