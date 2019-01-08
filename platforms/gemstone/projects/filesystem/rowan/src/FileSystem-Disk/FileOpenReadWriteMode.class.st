"
Open the file for both reading and writing
"
Class {
	#name : 'FileOpenReadWriteMode',
	#superclass : 'FileOpenMode',
	#category : 'FileSystem-Disk-Options'
}

{ #category : 'accessing' }
FileOpenReadWriteMode class >> defaultPlatformValue [
	^16r00002
]

{ #category : 'accessing' }
FileOpenReadWriteMode class >> posixName [
	^'O_RDWR'
]

{ #category : 'convenience' }
FileOpenReadWriteMode >> accessRule [
	^self store class readWriteAccessRule
]

{ #category : 'testing' }
FileOpenReadWriteMode >> isReadable [
	^true
]

{ #category : 'testing' }
FileOpenReadWriteMode >> isWritable [
	^true
]

{ #category : 'accessing' }
FileOpenReadWriteMode >> mode [
	"Truncate and Append are mutually exclusive"
	self parent hasFileTruncateOption ifTrue: [^'w+'].
	self parent hasFileAppendOption ifTrue: [^'a+'].
	^'r+'
]
