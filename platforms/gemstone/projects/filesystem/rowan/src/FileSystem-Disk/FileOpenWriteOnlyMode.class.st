"
Open the file for writing only
"
Class {
	#name : 'FileOpenWriteOnlyMode',
	#superclass : 'FileOpenMode',
	#category : 'FileSystem-Disk-Options'
}

{ #category : 'accessing' }
FileOpenWriteOnlyMode class >> defaultPlatformValue [
	^16r00001
]

{ #category : 'accessing' }
FileOpenWriteOnlyMode class >> posixName [
	^'O_WRONLY'
]

{ #category : 'convenience' }
FileOpenWriteOnlyMode >> accessRule [
	^self store class writeOnlyAccessRule
]

{ #category : 'testing' }
FileOpenWriteOnlyMode >> isWritable [
	^true
]

{ #category : 'accessing' }
FileOpenWriteOnlyMode >> mode [
	^self parent hasFileAppendOption 
		ifTrue: ['a'] 
		ifFalse: ['w']
]
