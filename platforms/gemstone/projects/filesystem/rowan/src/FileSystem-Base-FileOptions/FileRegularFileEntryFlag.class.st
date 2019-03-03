"
This flag is used to get regular file entries from the directory stream
"
Class {
	#name : 'FileRegularFileEntryFlag',
	#superclass : 'FileDirectoryStreamFlag',
	#category : 'FileSystem-Base-FileOptions'
}

{ #category : 'accessing' }
FileRegularFileEntryFlag class >> defaultPlatformValue [
	^1
]

{ #category : 'accessing' }
FileRegularFileEntryFlag class >> posixName [
	^'F_REG'
]

{ #category : 'accessing' }
FileRegularFileEntryFlag class >> windowsValue [
	^16r010
]
