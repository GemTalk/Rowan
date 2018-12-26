Class {
	#name : 'FileDenyReadWriteOption',
	#superclass : 'FileShareOption',
	#category : 'FileSystem-Disk-Options'
}

{ #category : 'access' }
FileDenyReadWriteOption class >> posixName [
	^'O_DENYRDWR'
]

{ #category : 'access' }
FileDenyReadWriteOption class >> posixValue [
	^16r00010
]
