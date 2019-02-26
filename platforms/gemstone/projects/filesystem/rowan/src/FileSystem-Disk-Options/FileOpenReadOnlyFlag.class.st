"
Open the file for reading only
"
Class {
	#name : 'FileOpenReadOnlyFlag',
	#superclass : 'FileOpenModeFlag',
	#category : 'FileSystem-Disk-Options'
}

{ #category : 'accessing' }
FileOpenReadOnlyFlag class >> defaultPlatformValue [
	^16r00000
]

{ #category : 'accessing' }
FileOpenReadOnlyFlag class >> posixName [
	^'O_RDONLY'
]

{ #category : 'convenience' }
FileOpenReadOnlyFlag >> accessRule [
	^self store class readOnlyAccessRule
]

{ #category : 'testing' }
FileOpenReadOnlyFlag >> isReadable [
	^true
]

{ #category : 'accessing' }
FileOpenReadOnlyFlag >> mode [
	^'r'
]
