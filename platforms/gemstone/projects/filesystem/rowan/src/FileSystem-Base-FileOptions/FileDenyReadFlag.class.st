"
Other processes can open the file for write-only access, but they cannot open it for read-only or read-write access.
"
Class {
	#name : 'FileDenyReadFlag',
	#superclass : 'FileShareFlag',
	#category : 'FileSystem-Base-FileOptions'
}

{ #category : 'access' }
FileDenyReadFlag class >> defaultPlatformValue [
	^16r00001
]

{ #category : 'access' }
FileDenyReadFlag class >> posixName [
	^'O_DENYRD'
]

{ #category : 'access' }
FileDenyReadFlag class >> windowsValue [
	^16r00030
]

{ #category : 'convenience' }
FileDenyReadFlag >> shareRule [
	^self store class denyReadShareRule
]
