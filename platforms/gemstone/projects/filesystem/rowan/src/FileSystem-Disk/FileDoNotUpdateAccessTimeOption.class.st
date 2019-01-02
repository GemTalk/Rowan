Class {
	#name : 'FileDoNotUpdateAccessTimeOption',
	#superclass : 'FileCreationOption',
	#category : 'FileSystem-Disk-Options'
}

{ #category : 'accessing' }
FileDoNotUpdateAccessTimeOption class >> posinName [
	^'O_NOATIME'
]

{ #category : 'accessing' }
FileDoNotUpdateAccessTimeOption class >> posixValue [
	self error: 'Not supported'
]
