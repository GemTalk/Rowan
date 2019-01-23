"
see https://linux.die.net/man/2/fcntl
"
Class {
	#name : 'FileGetLockFlag',
	#superclass : 'FileControlFlag',
	#category : 'FileSystem-Disk-Options'
}

{ #category : 'access' }
FileGetLockFlag class >> defaultPlatformValue [
	^14
]

{ #category : 'access' }
FileGetLockFlag class >> posixName [
	^'F_GETLK'
]
