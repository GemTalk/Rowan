"
Read the file descriptor flags; arg is ignored.
"
Class {
	#name : 'FileGetDescriptorFlag',
	#superclass : 'FileControlFlag',
	#category : 'FileSystem-Base-FileOptions'
}

{ #category : 'accessing' }
FileGetDescriptorFlag class >> defaultPlatformValue [
	^1
]

{ #category : 'accessing' }
FileGetDescriptorFlag class >> posixName [
	^'F_GETFD'
]
