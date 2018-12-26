Class {
	#name : 'FileDenyWriteOption',
	#superclass : 'FileShareOption',
	#category : 'FileSystem-Disk-Options'
}

{ #category : 'access' }
FileDenyWriteOption class >> posixName [
	^'O_DENYWR'
]

{ #category : 'access' }
FileDenyWriteOption class >> posixValue [
	^16r00020
]
