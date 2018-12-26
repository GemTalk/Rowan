Class {
	#name : 'FileDenyReadOption',
	#superclass : 'FileShareOption',
	#category : 'FileSystem-Disk-Options'
}

{ #category : 'access' }
FileDenyReadOption class >> posixName [
	^'O_DENYRD'
]

{ #category : 'access' }
FileDenyReadOption class >> posixValue [
	^16r00030
]
