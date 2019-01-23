"
Open the file for writing only
"
Class {
	#name : 'FileOpenWriteOnlyFlag',
	#superclass : 'FileOpenModeFlag',
	#category : 'FileSystem-Disk-Options'
}

{ #category : 'accessing' }
FileOpenWriteOnlyFlag class >> defaultPlatformValue [
	^16r00001
]

{ #category : 'accessing' }
FileOpenWriteOnlyFlag class >> posixName [
	^'O_WRONLY'
]

{ #category : 'convenience' }
FileOpenWriteOnlyFlag >> accessRule [
	^self store class writeOnlyAccessRule
]

{ #category : 'testing' }
FileOpenWriteOnlyFlag >> isWritable [
	^true
]

{ #category : 'accessing' }
FileOpenWriteOnlyFlag >> mode [
	^self parent hasFileAppendFlag
		ifTrue: ['a'] 
		ifFalse: ['w']
]
