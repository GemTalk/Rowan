"
This flag is used to get directory entries from the directory stream
"
Class {
	#name : 'FileDirectoryEntryFlag',
	#superclass : 'FileDirectoryStreamFlag',
	#category : 'FileSystem-Disk-Options'
}

{ #category : 'accessing' }
FileDirectoryEntryFlag class >> defaultPlatformValue [
	^2
]

{ #category : 'accessing' }
FileDirectoryEntryFlag class >> posixName [
	^'F_DIR'
]

{ #category : 'accessing' }
FileDirectoryEntryFlag class >> windowsValue [
	^16r100
]
