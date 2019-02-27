"
Specifies permission for users in the file owner class either:

    To search, if the file is a directory
    To execute the program in the file, for a file other than a directory.

In windows this option has no effect.
"
Class {
	#name : 'FileOwnerExecutePermissionFlag',
	#superclass : 'FilePermissionFlag',
	#category : 'FileSystem-Disk-Options'
}

{ #category : 'accessing' }
FileOwnerExecutePermissionFlag class >> defaultPlatformValue [
	^16r40
]

{ #category : 'accessing' }
FileOwnerExecutePermissionFlag class >> posixName [
	^'SIXUSR'
]

{ #category : 'accessing' }
FileOwnerExecutePermissionFlag class >> windowsValue [
	^0
]
