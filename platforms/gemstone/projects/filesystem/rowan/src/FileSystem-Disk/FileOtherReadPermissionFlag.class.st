"
Specifies permission for users in the file other class to read the file.
"
Class {
	#name : 'FileOtherReadPermissionFlag',
	#superclass : 'FilePermissionFlag',
	#category : 'FileSystem-Disk-Options'
}

{ #category : 'accessing' }
FileOtherReadPermissionFlag class >> defaultPlatformValue [
	^16r004
]

{ #category : 'accessing' }
FileOtherReadPermissionFlag class >> posixName [
	^'SIROTH'
]

{ #category : 'accessing' }
FileOtherReadPermissionFlag class >> windowsValue [
	^0
]
