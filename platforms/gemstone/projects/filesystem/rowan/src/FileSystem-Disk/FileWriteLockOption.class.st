Class {
	#name : 'FileWriteLockOption',
	#superclass : 'FileLockOption',
	#category : 'FileSystem-Disk-Options'
}

{ #category : 'accessing' }
FileWriteLockOption class >> posixName [
	^'FWRLOCK'
]

{ #category : 'accessing' }
FileWriteLockOption class >> posixValue [
	^2
]
