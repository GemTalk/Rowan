Class {
	#name : 'FileGetStatusFlag',
	#superclass : 'FileControlFlag',
	#category : 'FileSystem-Disk-Options'
}

{ #category : 'access' }
FileGetStatusFlag class >> defaultPlatformValue [
	^3
]

{ #category : 'access' }
FileGetStatusFlag class >> posixName [
	^'F_GETFL'
]
