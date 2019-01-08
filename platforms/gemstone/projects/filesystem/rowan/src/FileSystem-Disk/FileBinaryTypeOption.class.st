"
A binary file is basically any file that is not ""line-oriented"". Any file where besides the actual written characters and newlines there are other symbols as well.

No CR-LF translation: Explicitly opens the file in binary mode.

This is Windows only
"
Class {
	#name : 'FileBinaryTypeOption',
	#superclass : 'FileTypeOption',
	#category : 'FileSystem-Disk-Options'
}

{ #category : 'accessing' }
FileBinaryTypeOption class >> defaultPlatformValue [
	^0
]

{ #category : 'accessing' }
FileBinaryTypeOption class >> posixName [
	^'O_BINARY'
]

{ #category : 'accessing' }
FileBinaryTypeOption class >> windowsValue [
	^1
]

{ #category : 'printing' }
FileBinaryTypeOption >> printOpenModeStringOn: aStream [

	aStream nextPutAll: 'b'
]
