"
Specifies permission for the file owner to read the file.

For Windows, this returns a 1 for read.
"
Class {
	#name : 'FileOwnerReadPermissionFlag',
	#superclass : 'FilePermissionFlag',
	#category : 'FileSystem-Disk-Options'
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
