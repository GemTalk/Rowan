"
The current process has exclusive access to the file.
Other processes may not open the file.   It is unspecified whether
the file may be opened by the current process.
"
Class {
	#name : 'FileDenyReadWriteFlag',
	#superclass : 'FileShareFlag',
	#category : 'FileSystem-Disk-Options'
}

{ #category : 'access' }
FileDenyReadWriteFlag class >> defaultPlatformValue [
	^16r00002
]

{ #category : 'access' }
FileDenyReadWriteFlag class >> posixName [
	^'O_DENYRDWR'
]

{ #category : 'access' }
FileDenyReadWriteFlag class >> windowsValue [
	^16r00010
]

{ #category : 'convenience' }
FileDenyReadWriteFlag >> shareRule [
	^self store class denyReadWriteShareRule
]
