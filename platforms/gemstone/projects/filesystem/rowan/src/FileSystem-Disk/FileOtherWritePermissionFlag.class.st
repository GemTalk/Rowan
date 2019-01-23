"
Specifies permission for users in the file other class to write the file.
"
Class {
	#name : 'FileOtherWritePermissionFlag',
	#superclass : 'FilePermissionFlag',
	#category : 'FileSystem-Disk-Options'
}

{ #category : 'accessing' }
FileOtherWritePermissionFlag class >> defaultPlatformValue [
	^16r002
]

{ #category : 'accessing' }
FileOtherWritePermissionFlag class >> posixName [
	^'SIWOTH'
]

{ #category : 'accessing' }
FileOtherWritePermissionFlag class >> windowsValue [
	^0
]
