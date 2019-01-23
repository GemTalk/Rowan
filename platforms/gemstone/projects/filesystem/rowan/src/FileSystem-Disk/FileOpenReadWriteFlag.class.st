"
Open the file for both reading and writing
"
Class {
	#name : 'FileOpenReadWriteFlag',
	#superclass : 'FileOpenModeFlag',
	#category : 'FileSystem-Disk-Options'
}

{ #category : 'accessing' }
FileOpenReadWriteFlag class >> defaultPlatformValue [
	^16r00002
]

{ #category : 'accessing' }
FileOpenReadWriteFlag class >> posixName [
	^'O_RDWR'
]

{ #category : 'convenience' }
FileOpenReadWriteFlag >> accessRule [
	^self store class readWriteAccessRule
]

{ #category : 'testing' }
FileOpenReadWriteFlag >> isReadable [
	^true
]

{ #category : 'testing' }
FileOpenReadWriteFlag >> isWritable [
	^true
]

{ #category : 'accessing' }
FileOpenReadWriteFlag >> mode [
	"Truncate and Append are mutually exclusive"
	self parent hasFileTruncateFlag ifTrue: [^'w+'].
	self parent hasFileAppendFlag ifTrue: [^'a+'].
	^'r+'
]
