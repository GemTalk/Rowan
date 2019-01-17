Class {
	#name : 'FileOption',
	#superclass : 'Object',
	#instVars : [
		'parent'
	],
	#category : 'FileSystem-Disk-Options'
}

{ #category : 'querry' }
FileOption class >> allConcreteFileOptionClasses [ 
	^self allSubclasses select: [:ea | ea subclasses isEmpty and: [(ea class includesSelector: #defaultPlatformValue)]]
]

{ #category : 'accessing' }
FileOption class >> defaultPlatformValue [
	^self error: 'Not implmented'
]

{ #category : 'printing' }
FileOption class >> fileOptionsReport [
	" FileOption fileOptionsReport "
	| aStream |
	aStream := WriteStream on: String new.
	self reportOn: aStream indent: 0.
	^aStream contents
]

{ #category : 'accessing' }
FileOption class >> posixName [
	self subclassResponsibility
]

{ #category : 'querry' }
FileOption class >> posixNamesWithClasses [
	" FileOption posixNamesWithClasses"
	| aDictionary |
	aDictionary := Dictionary new.
	self allSubclasses do: [:ea | 
		(ea class includesSelector: #posixName) ifTrue: [aDictionary at: ea posixName put: ea] ].
	^aDictionary
]

{ #category : 'printing' }
FileOption class >> reportOn: aStream indent: indent [
	| aSize aValue |
	indent timesRepeat: [aStream tab].

	aStream nextPutAll: self name asString.
	(45 - ((indent * 4) + self name asString size)) timesRepeat: [aStream space].

	((self name = #FileOption) not and: [self class includesSelector: #posixName]) 
		ifTrue: [
			aSize := self posixName size.
			aStream nextPutAll: self posixName].
	(15 - (aSize ifNil: [0])) timesRepeat: [aStream space].

	((self name = #FileOption) not and: [self class includesSelector: #defaultPlatformValue]) 
		ifTrue: [aValue := self defaultPlatformValue asString]
		ifFalse: [aValue := (self subclasses isEmpty not) ifTrue: [''] ifFalse: ['value not defined']].
	aSize :=  aValue size.
	(15 - aSize) timesRepeat: [aStream space].
	aStream nextPutAll: aValue asString.

	((self name = #FileOption) not and: [self class includesSelector: #windowsValue]) 
		ifTrue: [
			aSize := self windowsValue asString size.
			(15 - aSize) timesRepeat: [aStream space].
			aStream nextPutAll: self windowsValue asString].

	aStream crlf.

	self subclasses do: [:ea | ea reportOn: aStream indent: indent + 1]
]

{ #category : 'accessing' }
FileOption class >> valueForSelector: valueSelector [
	^(self class canUnderstand: valueSelector) 
		ifTrue: [self perform: valueSelector] 
		ifFalse: [self defaultPlatformValue]
]

{ #category : 'accessing' }
FileOption >> optionValue [
	^self store optionPlatformValueFor: self class posixName
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
FileOption >> store [
	^self parent store
]
