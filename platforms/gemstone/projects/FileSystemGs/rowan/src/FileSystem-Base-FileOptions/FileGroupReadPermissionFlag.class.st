"
Specifies permission for users in the file group class to read the file.

If applied to a WindowsStore, this has no effect.
"
Class {
	#name : 'FileGroupReadPermissionFlag',
	#superclass : 'FilePermissionFlag',
	#category : 'FileSystem-Base-FileOptions'
}

{ #category : 'access' }
FileGroupReadPermissionFlag class >> defaultPlatformValue [
	^16r020
]

{ #category : 'access' }
FileGroupReadPermissionFlag class >> posixName [
	^'SIRGRP'
]

{ #category : 'access' }
FileGroupReadPermissionFlag class >> windowsValue [
	^0
]
