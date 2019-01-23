"
Specifies permission for users in the file group class either:

    To read, write, and search, if the file is a directory
    To read, write, and execute, for a file other than a directory

This value has the same effect as specifying all three parameters (SIRGRP, SIWGRP, and SIXGRP).
"
Class {
	#name : 'FileGroupAllPermissionFlag',
	#superclass : 'FilePermissionFlag',
	#category : 'FileSystem-Disk-Options'
}

{ #category : 'access' }
FileGroupAllPermissionFlag class >> defaultPlatformValue [
	^16r20
]

{ #category : 'access' }
FileGroupAllPermissionFlag class >> posixName [
	^'SIRWXG'
]

{ #category : 'access' }
FileGroupAllPermissionFlag class >> windowsValue [
	^0
]
