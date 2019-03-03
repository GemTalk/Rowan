"
Exclusive MANDATORY lock.

Windows supports this type of lock.
"
Class {
	#name : 'FileMandatoryLockFlag',
	#superclass : 'FileLockFlag',
	#category : 'FileSystem-Base-FileOptions'
}

{ #category : 'accessing' }
FileMandatoryLockFlag class >> defaultPlatformValue [
	^0
]

{ #category : 'accessing' }
FileMandatoryLockFlag class >> posixName [
	^'F_MDLCK'
]
