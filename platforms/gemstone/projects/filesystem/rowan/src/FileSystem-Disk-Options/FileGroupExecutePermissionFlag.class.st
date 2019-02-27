"
Specifies permission for users in the file group class either:

    To search, if the file is a directory
    To execute the program in the file, for a file other than a directory.

If applied to a WindowsStore, this has no effect.
"
Class {
	#name : 'FileGroupExecutePermissionFlag',
	#superclass : 'FilePermissionFlag',
	#category : 'FileSystem-Disk-Options'
}

{ #category : 'access' }
FileGroupExecutePermissionFlag class >> defaultPlatformValue [
	^16r008
]

{ #category : 'access' }
FileGroupExecutePermissionFlag class >> posixName [
	^'SIXGRP'
]

{ #category : 'access' }
FileGroupExecutePermissionFlag class >> windowsValue [
	^0
]
