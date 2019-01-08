"
This is an abstract class whose subclasses define the file type. These are applicable only to windows operating system.

The ""posixValue"" in Unix/Linux will always be zero.
"
Class {
	#name : 'FileAppendOption',
	#superclass : 'FileStatusOption',
	#category : 'FileSystem-Disk-Options'
}

{ #category : 'accessing' }
FileAppendOption class >> defaultPlatformValue [
	^16r00400
]

{ #category : 'accessing' }
FileAppendOption class >> posixName [
	^'O_APPEND'
]

{ #category : 'accessing' }
FileAppendOption class >> windowsValue [
	^16r10000
]
