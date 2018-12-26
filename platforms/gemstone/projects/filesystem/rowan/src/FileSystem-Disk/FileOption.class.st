Class {
	#name : 'FileOption',
	#superclass : 'Object',
	#instVars : [
		'parent'
	],
	#category : 'FileSystem-Disk-Options'
}

{ #category : 'accessing' }
FileOption class >> posixName [
	self subclassResponsibility
]

{ #category : 'accessing' }
FileOption >> parent [
	^parent
]

{ #category : 'accessing' }
FileOption >> parent: anObject [
	parent := anObject
]

{ #category : 'accessing' }
FileOption >> posixName [
	^self class posixName
]

{ #category : 'accessing' }
FileOption >> posixValue [
	^self class posixValue
]
