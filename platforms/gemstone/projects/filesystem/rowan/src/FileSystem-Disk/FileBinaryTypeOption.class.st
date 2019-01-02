"
A binary file is basically any file that is not ""line-oriented"". Any file where besides the actual written characters and newlines there are other symbols as well.

No CR-LF translation: Explicitly opens the file in binary mode.

This is Windows only
"
Class {
	#name : 'FileBinaryTypeOption',
	#superclass : 'FileFileTypeOption',
	#category : 'FileSystem-Disk-Options'
}

{ #category : 'posixName' }
FileBinaryTypeOption class >> posixName [
	^'O_BINARY'
]

{ #category : 'posixName' }
FileBinaryTypeOption class >> posixValue [
	^0
]

{ #category : 'printing' }
FileBinaryTypeOption >> printOpenModeStringOn: aStream [

	aStream nextPutAll: 'b'
]
