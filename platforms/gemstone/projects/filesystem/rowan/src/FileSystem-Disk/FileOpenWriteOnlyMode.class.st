"
Open the file for writing only
"
Class {
	#name : 'FileOpenWriteOnlyMode',
	#superclass : 'FileOpenMode',
	#category : 'FileSystem-Disk-Options'
}

{ #category : 'accessing' }
FileOpenWriteOnlyMode class >> posixName [
	^'O_WRONLY'
]

{ #category : 'accessing' }
FileOpenWriteOnlyMode class >> posixValue [
	^16r00001
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
