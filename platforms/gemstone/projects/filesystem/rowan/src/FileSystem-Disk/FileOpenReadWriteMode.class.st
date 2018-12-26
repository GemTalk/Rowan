"
Open the file for both reading and writing
"
Class {
	#name : 'FileOpenReadWriteMode',
	#superclass : 'FileOpenMode',
	#category : 'FileSystem-Disk-Options'
}

{ #category : 'accessing' }
FileOpenReadWriteMode class >> posixName [
	^'O_RDWR'
]

{ #category : 'accessing' }
FileOpenReadWriteMode class >> posixValue [
	^16r00002
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
