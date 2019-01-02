Class {
	#name : 'FileFileTypeOption',
	#superclass : 'FileOption',
	#category : 'FileSystem-Disk-Options'
}

{ #category : 'printing' }
FileFileTypeOption >> printOpenModeStringOn: aStream [

	self fileOptions printOpenModeStringOn: aStream.
]
