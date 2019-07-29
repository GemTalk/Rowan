"
Remove our lease from the file.
"
Class {
	#name : 'FileSetUnlockFlag',
	#superclass : 'FileControlFlag',
	#category : 'FileSystem-Base-FileOptions'
}

{ #category : 'accessing' }
FileSetUnlockFlag class >> defaultPlatformValue [
	^3
]

{ #category : 'accessing' }
FileSetUnlockFlag class >> posixName [
	^'F_UNLCK'
]
