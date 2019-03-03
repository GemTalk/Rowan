"
Specifies permission for users in the file owner class either:

    To write to the file or directory

For Windows, this returns a 2.
"
Class {
	#name : 'FileOwnerWritePermissionFlag',
	#superclass : 'FilePermissionFlag',
	#category : 'FileSystem-Base-FileOptions'
}

{ #category : 'accessing' }
FileOwnerWritePermissionFlag class >> defaultPlatformValue [
	^16r80
]

{ #category : 'accessing' }
FileOwnerWritePermissionFlag class >> posixName [
	^'SIWUSR'
]

{ #category : 'accessing' }
FileOwnerWritePermissionFlag class >> windowsValue [
	^2
]
