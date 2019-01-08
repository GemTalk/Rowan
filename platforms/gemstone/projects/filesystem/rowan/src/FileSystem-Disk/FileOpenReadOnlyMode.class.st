"
Open the file for reading only
"
Class {
	#name : 'FileOpenReadOnlyMode',
	#superclass : 'FileOpenMode',
	#category : 'FileSystem-Disk-Options'
}

{ #category : 'accessing' }
FileOpenReadOnlyMode class >> defaultPlatformValue [
	^16r00000
]

{ #category : 'accessing' }
FileOpenReadOnlyMode class >> posixName [
	^'O_RDONLY'
]

{ #category : 'convenience' }
FileOpenReadOnlyMode >> accessRule [
	^self store class readOnlyAccessRule
]

{ #category : 'testing' }
FileOpenReadOnlyMode >> isReadable [
	^true
]

{ #category : 'accessing' }
FileOpenReadOnlyMode >> mode [
	^'r'
]
