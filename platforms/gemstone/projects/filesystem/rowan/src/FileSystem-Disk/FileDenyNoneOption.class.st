Class {
	#name : 'FileDenyNoneOption',
	#superclass : 'FileShareOption',
	#category : 'FileSystem-Disk-Options'
}

{ #category : 'access' }
FileDenyNoneOption class >> posixName [
	^'O_DENYNONE'
]

{ #category : 'access' }
FileDenyNoneOption class >> posixValue [
	^16r00040
]
