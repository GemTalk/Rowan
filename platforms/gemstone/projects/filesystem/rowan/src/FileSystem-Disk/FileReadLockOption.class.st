Class {
	#name : 'FileReadLockOption',
	#superclass : 'FileLockOption',
	#category : 'FileSystem-Disk-Options'
}

{ #category : 'accessing' }
FileReadLockOption class >> posixName [
	^'FRDLOCK'
]

{ #category : 'accessing' }
FileReadLockOption class >> posixValue [
	^1
]
