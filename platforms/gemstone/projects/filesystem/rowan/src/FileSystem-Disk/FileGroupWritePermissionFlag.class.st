"
Specifies permission for users in the file owner class either:

    To search, if the file is a directory
    To execute the program in the file, for a file other than a directory
"
Class {
	#name : 'FileGroupWritePermissionFlag',
	#superclass : 'FilePermissionFlag',
	#category : 'FileSystem-Disk-Options'
}

{ #category : 'access' }
FileGroupWritePermissionFlag class >> defaultPlatformValue [
	^16r010
]

{ #category : 'access' }
FileGroupWritePermissionFlag class >> posixName [
	^'SIWGRP'
]

{ #category : 'access' }
FileGroupWritePermissionFlag class >> windowsValue [
	^0
]
