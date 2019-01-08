Class {
	#name : 'FileDenyWriteOption',
	#superclass : 'FileShareOption',
	#category : 'FileSystem-Disk-Options'
}

{ #category : 'access' }
FileDenyWriteOption class >> defaultPlatformValue [
	^16r00003
]

{ #category : 'access' }
FileDenyWriteOption class >> posixName [
	^'O_DENYWR'
]

{ #category : 'access' }
FileDenyWriteOption class >> windowsValue [
	^16r00020
]

{ #category : 'convenience' }
FileDenyWriteOption >> shareRule [
	^self store class denyWriteShareRule
]
