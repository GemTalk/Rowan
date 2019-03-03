"
Specifies permission for users in the file owner class either:

    To read the file or directory

For Windows, this returns a 1 for read.
"
Class {
	#name : 'FileOwnerReadPermissionFlag',
	#superclass : 'FilePermissionFlag',
	#category : 'FileSystem-Base-FileOptions'
}

{ #category : 'accessing' }
FileOwnerReadPermissionFlag class >> defaultPlatformValue [
	^16r100
]

{ #category : 'accessing' }
FileOwnerReadPermissionFlag class >> posixName [
	^'SIRUSR'
]

{ #category : 'accessing' }
FileOwnerReadPermissionFlag class >> windowsValue [
	^1
]
