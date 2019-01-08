Class {
	#name : 'FileShareOption',
	#superclass : 'FileOption',
	#category : 'FileSystem-Disk-Options'
}

{ #category : 'convenience' }
FileShareOption >> shareRule [
	^self subclassResponsibility
]
