Class {
	#name : 'FileTypeOption',
	#superclass : 'FileOption',
	#category : 'FileSystem-Disk-Options'
}

{ #category : 'testing' }
FileTypeOption >> isGzipped [
	^false
]

{ #category : 'printing' }
FileTypeOption >> printOpenModeStringOn: aStream [

	self fileOptions printOpenModeStringOn: aStream.
]
