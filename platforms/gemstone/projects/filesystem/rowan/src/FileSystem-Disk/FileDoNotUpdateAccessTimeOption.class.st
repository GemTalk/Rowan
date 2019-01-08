Class {
	#name : 'FileDoNotUpdateAccessTimeOption',
	#superclass : 'FileCreationOption',
	#category : 'FileSystem-Disk-Options'
}

{ #category : 'accessing' }
FileDoNotUpdateAccessTimeOption class >> posixName [
	^'O_NOATIME'
]

{ #category : 'accessing' }
FileDoNotUpdateAccessTimeOption >> posixName [
		^'O_NOATIME'
]
