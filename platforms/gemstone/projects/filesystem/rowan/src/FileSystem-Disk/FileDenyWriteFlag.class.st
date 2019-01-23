"
Other processes can open the file for read-only access, but they cannot open it for write-only or read-write access.
"
Class {
	#name : 'FileDenyWriteFlag',
	#superclass : 'FileShareFlag',
	#category : 'FileSystem-Disk-Options'
}

{ #category : 'access' }
FileDenyWriteFlag class >> defaultPlatformValue [
	^16r00003
]

{ #category : 'access' }
FileDenyWriteFlag class >> posixName [
	^'O_DENYWR'
]

{ #category : 'access' }
FileDenyWriteFlag class >> windowsValue [
	^16r00020
]

{ #category : 'convenience' }
FileDenyWriteFlag >> shareRule [
	^self store class denyWriteShareRule
]
