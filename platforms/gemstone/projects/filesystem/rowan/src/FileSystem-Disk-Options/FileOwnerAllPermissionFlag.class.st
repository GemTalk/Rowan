"
Specifies permission for the file owner either:

-    To read, write, and search, if the file is a directory
-    To read, write, and execute, for a file other than a directory

This value has the same effect as specifying all three parameters (SIRUSR, SIWUSR, and SIXUSR)
"
Class {
	#name : 'FileOwnerAllPermissionFlag',
	#superclass : 'FilePermissionFlag',
	#category : 'FileSystem-Disk-Options'
}

{ #category : 'accessing' }
FileOwnerAllPermissionFlag class >> defaultPlatformValue [
	^16r1C0
]

{ #category : 'accessing' }
FileOwnerAllPermissionFlag class >> posixName [
	^'SIRWXU'
]

{ #category : 'accessing' }
FileOwnerAllPermissionFlag class >> windowsValue [
	^3
]
