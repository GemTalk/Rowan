"
Other processes can open the file for any access: read-only, write-only, or read-write.
"
Class {
	#name : 'FileDenyNoneFlag',
	#superclass : 'FileShareFlag',
	#category : 'FileSystem-Base-FileOptions'
}

{ #category : 'access' }
FileDenyNoneFlag class >> defaultPlatformValue [
	^16r00000
]

{ #category : 'access' }
FileDenyNoneFlag class >> posixName [
	^'O_DENYNONE'
]

{ #category : 'access' }
FileDenyNoneFlag class >> windowsValue [
	^16r00040
]

{ #category : 'convenience' }
FileDenyNoneFlag >> shareRule [
	^self store class denyNoneShareRule
]
