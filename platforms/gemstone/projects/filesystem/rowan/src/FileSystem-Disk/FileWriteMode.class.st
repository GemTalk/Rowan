"
Truncate file to zero length or create text file for writing. 
The stream is positioned at the beginning of the file.
"
Class {
	#name : 'FileWriteMode',
	#superclass : 'FileMode',
	#category : 'FileSystem-Disk-Base'
}

{ #category : 'testing' }
FileWriteMode >> isWritable [
	^true
]

{ #category : 'accessing' }
FileWriteMode >> mode [
	"Truncate file to zero length or create text file for writing. 
	 The stream is positioned at the beginning of the file."
	^'w'
]
