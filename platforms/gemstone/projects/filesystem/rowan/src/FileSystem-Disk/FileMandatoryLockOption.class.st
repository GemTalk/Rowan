Class {
	#name : 'FileMandatoryLockOption',
	#superclass : 'FileLockOption',
	#category : 'FileSystem-Disk-Options'
}

{ #category : 'accessing' }
FileMandatoryLockOption class >> posixName [
	^'FMDLOCK'
]

{ #category : 'accessing' }
FileMandatoryLockOption class >> posixValue [
	^0
]
