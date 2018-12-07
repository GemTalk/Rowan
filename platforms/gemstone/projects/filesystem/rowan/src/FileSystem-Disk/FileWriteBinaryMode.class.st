Class {
	#name : 'FileWriteBinaryMode',
	#superclass : 'FileWriteMode',
	#category : 'FileSystem-Disk-Base'
}

{ #category : 'accessing' }
FileWriteBinaryMode >> mode [
	"Truncate binary file to zero length or create text file for writing. 
	 The stream is positioned at the beginning of the file."
	^'wb'
]
