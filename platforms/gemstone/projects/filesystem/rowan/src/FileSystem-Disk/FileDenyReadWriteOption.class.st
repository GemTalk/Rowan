Class {
	#name : 'FileDenyReadWriteOption',
	#superclass : 'FileShareOption',
	#category : 'FileSystem-Disk-Options'
}

{ #category : 'access' }
FileDenyReadWriteOption class >> defaultPlatformValue [
	^16r00002
]

{ #category : 'access' }
FileDenyReadWriteOption class >> posixName [
	^'O_DENYRDWR'
]

{ #category : 'access' }
FileDenyReadWriteOption class >> windowsValue [
	^16r00010
]

{ #category : 'convenience' }
FileDenyReadWriteOption >> shareRule [
	^self store class denyReadWriteShareRule
]
