"
A binary file is basically any file that is not ""line-oriented"". Any file where besides the actual written characters and newlines there are other symbols as well.

No CR-LF translation: Explicitly opens the file in binary mode.

This is Windows only
"
Class {
	#name : 'FileBinaryTypeFlag',
	#superclass : 'FileTypeFlag',
	#category : 'FileSystem-Base-FileOptions'
}

{ #category : 'accessing' }
FileBinaryTypeFlag class >> defaultPlatformValue [
	^0
]

{ #category : 'accessing' }
FileBinaryTypeFlag class >> posixName [
	^'O_BINARY'
]

{ #category : 'accessing' }
FileBinaryTypeFlag class >> windowsValue [
	^1
]

{ #category : 'printing' }
FileBinaryTypeFlag >> printOpenModeStringOn: aStream [

	aStream nextPutAll: 'b'
]
