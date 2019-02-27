"
Specifies permission for users in the file other class either:

    To search, if the file is a directory
    To execute the program in the file, for a file other than a directory.

If applied to a WindowsStore, this has no effect.
"
Class {
	#name : 'FileOtherExecutePermissionFlag',
	#superclass : 'FilePermissionFlag',
	#category : 'FileSystem-Disk-Options'
}

{ #category : 'accessing' }
FileOtherExecutePermissionFlag class >> defaultPlatformValue [
	^16r001
]

{ #category : 'accessing' }
FileOtherExecutePermissionFlag class >> posixName [
	^'SIXOTH'
]

{ #category : 'accessing' }
FileOtherExecutePermissionFlag class >> windowsValue [
	^0
]
