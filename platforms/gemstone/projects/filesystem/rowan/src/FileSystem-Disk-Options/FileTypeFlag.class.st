"
Abstract class to support file options. These are used when opening the file.
"
Class {
	#name : 'FileTypeFlag',
	#superclass : 'PosixFlag',
	#category : 'FileSystem-Disk-Options'
}

{ #category : 'testing' }
FileTypeFlag >> isGzipped [
	^false
]

{ #category : 'printing' }
FileTypeFlag >> printOpenModeStringOn: aStream [

	self fileOptions printOpenModeStringOn: aStream.
]
