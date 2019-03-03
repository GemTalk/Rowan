"
These are file locking flags for use in Linux
"
Class {
	#name : 'FileUnlockFlag',
	#superclass : 'FileLockFlag',
	#category : 'FileSystem-Base-FileOptions'
}

{ #category : 'accessing' }
FileUnlockFlag class >> defaultPlatformValue [
	^3
]

{ #category : 'accessing' }
FileUnlockFlag class >> posixName [
	^'F_UNLCK'
]
