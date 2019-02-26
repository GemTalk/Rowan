"
Specifies permission for the file owner to write the file.

For windows this returns 2
"
Class {
	#name : 'FileOwnerWritePermissionFlag',
	#superclass : 'FilePermissionFlag',
	#category : 'FileSystem-Disk-Options'
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
