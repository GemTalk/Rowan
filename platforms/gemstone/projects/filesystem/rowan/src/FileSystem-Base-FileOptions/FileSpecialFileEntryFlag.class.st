"
The subclasses are flags for filtering the items returned from the directory stream.
Specifying all three flags will result in all entries in the directory stream being returned.
"
Class {
	#name : 'FileSpecialFileEntryFlag',
	#superclass : 'FileDirectoryStreamFlag',
	#category : 'FileSystem-Base-FileOptions'
}

{ #category : 'accessing' }
FileSpecialFileEntryFlag class >> defaultPlatformValue [
	^4
]

{ #category : 'accessing' }
FileSpecialFileEntryFlag class >> posixName [
	^'F_SPECIAL'
]

{ #category : 'accessing' }
FileSpecialFileEntryFlag class >> windowsValue [
	^16r006
]
