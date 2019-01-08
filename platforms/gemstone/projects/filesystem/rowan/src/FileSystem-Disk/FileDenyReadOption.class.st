Class {
	#name : 'FileDenyReadOption',
	#superclass : 'FileShareOption',
	#category : 'FileSystem-Disk-Options'
}

{ #category : 'access' }
FileDenyReadOption class >> defaultPlatformValue [
	^16r00001
]

{ #category : 'access' }
FileDenyReadOption class >> posixName [
	^'O_DENYRD'
]

{ #category : 'access' }
FileDenyReadOption class >> windowsValue [
	^16r00030
]

{ #category : 'convenience' }
FileDenyReadOption >> shareRule [
	^self store class denyReadShareRule
]
