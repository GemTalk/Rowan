"
This is an abstract class whose subclasses define the file type. These are applicable only to windows operating system.

The ""posixValue"" in Unix/Linux will always be zero.
"
Class {
	#name : 'FileAppendFlag',
	#superclass : 'FileOpeningFlag',
	#category : 'FileSystem-Disk-Options'
}

{ #category : 'accessing' }
FileAppendFlag class >> defaultPlatformValue [
	^16r00400
]

{ #category : 'accessing' }
FileAppendFlag class >> posixName [
	^'O_APPEND'
]

{ #category : 'accessing' }
FileAppendFlag class >> windowsValue [
	^16r10000
]
