"
Exclusive MANDATORY lock.


Windows supports this type of lock.
"
Class {
	#name : 'FileMandatoryLockOption',
	#superclass : 'FileLockOption',
	#category : 'FileSystem-Disk-Options'
}

{ #category : 'accessing' }
FileMandatoryLockOption class >> defaultPlatformValue [
	^0
]

{ #category : 'accessing' }
FileMandatoryLockOption class >> posixName [
	^'F_MDLCK'
]
