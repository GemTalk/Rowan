Class {
	#name : 'FileExclusiveFlag',
	#superclass : 'FileOpeningFlag',
	#category : 'FileSystem-Disk-Options'
}

{ #category : 'accessing' }
FileExclusiveFlag class >> defaultPlatformValue [
	^16r0080
]

{ #category : 'accessing' }
FileExclusiveFlag class >> posixName [
	^'O_EXCL'
]

{ #category : 'accessing' }
FileExclusiveFlag class >> windowsValue [
	^16r40000
]
