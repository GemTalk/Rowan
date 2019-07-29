Class {
	#name : 'FileGetStatusFlag',
	#superclass : 'FileControlFlag',
	#category : 'FileSystem-Base-FileOptions'
}

{ #category : 'access' }
FileGetStatusFlag class >> defaultPlatformValue [
	^3
]

{ #category : 'access' }
FileGetStatusFlag class >> posixName [
	^'F_GETFL'
]
