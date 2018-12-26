Class {
	#name : 'FileDoNotUpdateAccessTimeOption',
	#superclass : 'FileCreationOption',
	#category : 'FileSystem-Disk-Options'
}

{ #category : 'accessing' }
FileDoNotUpdateAccessTimeOption class >> posinName [
	^'O_NOATIME'
]
