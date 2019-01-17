Class {
	#name : 'FilePosixError',
	#superclass : 'FileSystemError',
	#category : 'FileSystem-Disk-Errors'
}

{ #category : 'querry' }
FilePosixError class >> classWithPosixName: aString [
	" FilePosixError classWithPosixName: 'EINVAL' "
	^self allSubclasses detect: [:ea | 
		(ea class includesSelector: #posixName) and: [ea posixName = aString]] ifNone: []
]

{ #category : 'access' }
FilePosixError class >> defaultErrorNumber [
	self subclassResponsibility
]

{ #category : 'access' }
FilePosixError class >> errorDescription [
	self subclassResponsibility
]

{ #category : 'printing' }
FilePosixError class >> posixErrorsReport [
	" FilePosixError posixErrorsReport "
	| aStream |
	aStream := WriteStream on: String new.
	self reportOn: aStream indent: 0.
	^aStream contents
]

{ #category : 'access' }
FilePosixError class >> posixName [
	self subclassResponsibility
]

{ #category : 'querry' }
FilePosixError class >> posixNamesWithClasses [
	" FilePosixError posixNamesWithClasses"
	| aDictionary |
	aDictionary := Dictionary new.
	self allSubclasses do: [:ea | 
		(ea class includesSelector: #posixName) ifTrue: [aDictionary at: ea posixName put: ea] ].
	^aDictionary
]

{ #category : 'printing' }
FilePosixError class >> reportOn: aStream indent: indent [
	| aSize aValue |
	indent timesRepeat: [aStream tab].

	aStream nextPutAll: self name asString.
	(45 - ((indent * 4) + self name asString size)) timesRepeat: [aStream space].

	((self name = #FilePosixError) not and: [self class includesSelector: #posixName]) 
		ifTrue: [
			aSize := self posixName size.
			aStream nextPutAll: self posixName].
	(15 - (aSize ifNil: [0])) timesRepeat: [aStream space].

	((self name = #FilePosixError) not and: [self class includesSelector: #defaultErrorNumber]) 
		ifTrue: [aValue := self defaultErrorNumber asString]
		ifFalse: [aValue := (self subclasses isEmpty not) ifTrue: [''] ifFalse: ['value not defined']].
	aSize :=  aValue size.
	(15 - aSize) timesRepeat: [aStream space].
	aStream nextPutAll: aValue asString.

	((self name = #FileOption) not and: [self class includesSelector: #windowsErrorNumber]) 
		ifTrue: [
			aSize := self windowsErrorNumber asString size.
			(15 - aSize) timesRepeat: [aStream space].
			aStream nextPutAll: self windowsErrorNumber asString].

	aStream crlf.

	self subclasses do: [:ea | ea reportOn: aStream indent: indent + 1]
]
