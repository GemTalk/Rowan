"
This an abstract class. All POSIX flags are to be implemented as subclasses of this class.

Each concrete subclass should implement the following class methods:
- #posixName - the posix name - ex: O_RDONLY.
- #defaultPlatformValue - the default (Linux) value of the flag - ex: 0 for O_RDONLY.
- #windowsValue - optional - if the Windows value is different from the Linux value, implement this method.
"
Class {
	#name : 'PosixFlag',
	#superclass : 'Object',
	#instVars : [
		'parent'
	],
	#category : 'FileSystem-Disk-Options'
}

{ #category : 'querry' }
PosixFlag class >> allConcretePosixFlagClasses [ 
	^self allSubclasses select: [:ea | ea subclasses isEmpty and: [(ea class includesSelector: #defaultPlatformValue)]]
]

{ #category : 'accessing' }
PosixFlag class >> defaultPlatformValue [
	^self error: 'Not implmented'
]

{ #category : 'printing' }
PosixFlag class >> posixFlagsReport [
	" PosixFlag posixFlagsReport "
	| aStream |
	aStream := WriteStream on: String new.
	self reportOn: aStream indent: 0.
	^aStream contents
]

{ #category : 'accessing' }
PosixFlag class >> posixName [
	self subclassResponsibility
]

{ #category : 'querry' }
PosixFlag class >> posixNamesWithClasses [
	" PosixFlag posixNamesWithClasses"
	| aDictionary |
	aDictionary := Dictionary new.
	self allSubclasses do: [:ea | 
		(ea class includesSelector: #posixName) ifTrue: [aDictionary at: ea posixName put: ea] ].
	^aDictionary
]

{ #category : 'private' }
PosixFlag class >> reportOn: aStream indent: indent [
	| aSize aValue |
	indent timesRepeat: [aStream tab].

	aStream nextPutAll: self name asString.
	"(45 - ((indent * 4) + self name asString size)) timesRepeat: [aStream space]."
	aStream tab.

	((self name = #PosixFlag) not and: [self class includesSelector: #posixName]) 
		ifTrue: [
			aSize := self posixName size.
			aStream nextPutAll: self posixName].
	"(15 - (aSize ifNil: [0])) timesRepeat: [aStream space]."
	aStream tab.

	((self name = #PosixFlag) not and: [self class includesSelector: #defaultPlatformValue]) 
		ifTrue: [aValue := self defaultPlatformValue asString]
		ifFalse: [aValue := (self subclasses isEmpty not) ifTrue: [''] ifFalse: ['value not defined']].
	aSize :=  aValue size.
	"(15 - aSize) timesRepeat: [aStream space]."
	aStream tab.
	aStream nextPutAll: aValue asString.

	((self name = #PosixFlag) not and: [self class includesSelector: #windowsValue]) 
		ifTrue: [
			aSize := self windowsValue asString size.
			"(15 - aSize) timesRepeat: [aStream space]."
			aStream tab.
			aStream nextPutAll: self windowsValue asString].

	aStream crlf.

	self subclasses do: [:ea | ea reportOn: aStream indent: indent + 1]
]

{ #category : 'querry' }
PosixFlag class >> valueForSelector: valueSelector [
	^(self class canUnderstand: valueSelector) 
		ifTrue: [self perform: valueSelector] 
		ifFalse: [self defaultPlatformValue]
]

{ #category : 'accessing' }
PosixFlag >> parent [
	^parent
]

{ #category : 'accessing' }
PosixFlag >> parent: anObject [
	parent := anObject
]

{ #category : 'querry' }
PosixFlag >> posixFlagValue [
	^self store optionPlatformValueFor: self class posixName
]

{ #category : 'querry' }
PosixFlag >> posixName [
	^self class posixName
]

{ #category : 'querry' }
PosixFlag >> store [
	^self parent store
]
