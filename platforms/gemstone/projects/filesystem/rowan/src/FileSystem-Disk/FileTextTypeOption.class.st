"
CR-LF translation: Explicitly opens the file in text mode
"
Class {
	#name : 'FileTextTypeOption',
	#superclass : 'FileFileTypeOption',
	#category : 'FileSystem-Disk-Options'
}

{ #category : 'posixName' }
FileTextTypeOption class >> posixName [
	^'O_TEXT'
]

{ #category : 'posixName' }
FileTextTypeOption class >> posixValue [
	^0
]
