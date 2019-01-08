Class {
	#name : 'FileDenyNoneOption',
	#superclass : 'FileShareOption',
	#category : 'FileSystem-Disk-Options'
}

{ #category : 'access' }
FileDenyNoneOption class >> defaultPlatformValue [
	^16r00000
]

{ #category : 'access' }
FileDenyNoneOption class >> posixName [
	^'O_DENYNONE'
]

{ #category : 'access' }
FileDenyNoneOption class >> windowsValue [
	^16r00040
]

{ #category : 'convenience' }
FileDenyNoneOption >> shareRule [
	^self store class denyNoneShareRule
]
