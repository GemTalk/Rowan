"
As for F_SETLK, but if a conflicting lock is held on the file, then wait for that lock to be released. If a signal is caught while waiting, then the call is interrupted and (after the signal handler has returned) returns immediately (with return value -1 and errno set to EINTR; see signal(7)).
"
Class {
	#name : 'FileSetLockWaitFlag',
	#superclass : 'FileControlFlag',
	#category : 'FileSystem-Base-FileOptions'
}

{ #category : 'accessing' }
FileSetLockWaitFlag class >> defaultPlatformValue [
	^7
]

{ #category : 'accessing' }
FileSetLockWaitFlag class >> posixName [
	^'F_SETLKW'
]
