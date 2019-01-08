"
CR-LF translation: Explicitly opens the file in text mode
"
Class {
	#name : 'FileTextTypeOption',
	#superclass : 'FileTypeOption',
	#category : 'FileSystem-Disk-Options'
}

{ #category : 'accessing' }
FileTextTypeOption class >> defaultPlatformValue [
	^0
]

{ #category : 'accessing' }
FileTextTypeOption class >> posixName [
	^'O_TEXT'
]
