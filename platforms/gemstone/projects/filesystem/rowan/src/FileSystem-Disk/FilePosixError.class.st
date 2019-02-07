"
This is the abstract class for File/Directory Posix Errors.

Instance Vareables:
platformErrorNumber - the platform error number which resulted in this error.
errorGroupName - this gives the context of the error.
sourceReference - this is only used for copy/rename actions - it is the source reference. The target reference is stored in the reference instance variable.
options - if the error had options, this is where the options are stored.
"
Class {
	#name : 'FilePosixError',
	#superclass : 'FileSystemError',
	#instVars : [
		'platformErrorNumber',
		'errorGroupName',
		'sourceReference',
		'options'
	],
	#category : 'FileSystem-Disk-Errors'
}

{ #category : 'querry' }
FilePosixError class >> allConcreteFilePosixErrorClasses [
	^self allSubclasses select: [:ea | 
		ea subclasses isEmpty and: [(ea class includesSelector: #defaultErrorNumber)]]
]

{ #category : 'querry' }
FilePosixError class >> classWithPosixName: aString [
	" FilePosixError classWithPosixName: 'EINVAL' "
	^self allSubclasses detect: [:ea | 
		(ea class includesSelector: #posixName) and: [ea posixName = aString]] ifNone: []
]

{ #category : 'querry' }
FilePosixError class >> classWithPosixNumber: aString [
	" (FilePosixError classWithPosixNumber: 18) posixName "
	^self allSubclasses detect: [:ea | 
		(ea class includesSelector: #defaultErrorNumber) and: [ea defaultErrorNumber = aString]] ifNone: []
]

{ #category : 'access' }
FilePosixError class >> defaultErrorNumber [
	self subclassResponsibility
]

{ #category : 'access' }
FilePosixError class >> errorDescription [
	self subclassResponsibility
]

{ #category : 'querry' }
FilePosixError class >> errorForPlatformError: aNumber [

	^self
]

{ #category : 'reporting' }
FilePosixError class >> errorNumberForSelector: valueSelector [
	^(self class canUnderstand: valueSelector) 
		ifTrue: [self perform: valueSelector] 
		ifFalse: [self defaultErrorNumber]
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

	indent timesRepeat: [aStream tab].
	aStream nextPutAll: self name; tab; cr.

	(self name = #FilePosixError) 
		ifFalse: [
			aStream 
				nextPutAll: self posixName;
				tab;
				nextPutAll: self defaultErrorNumber asString;
				cr].
	self subclasses do: [:ea | ea reportOn: aStream indent: indent + 1]
]

{ #category : 'accessing' }
FilePosixError >> errorGroupName [
	^errorGroupName
]

{ #category : 'accessing' }
FilePosixError >> errorGroupName: aString [
	errorGroupName := aString
]

{ #category : 'accessing' }
FilePosixError >> options [
	^options
]

{ #category : 'accessing' }
FilePosixError >> options: anObject [
	options := anObject
]

{ #category : 'accessing' }
FilePosixError >> platformErrorNumber [
	^platformErrorNumber
]

{ #category : 'accessing' }
FilePosixError >> platformErrorNumber: anInteger [
	platformErrorNumber := anInteger
]

{ #category : 'accessing' }
FilePosixError >> sourceReference [
	^sourceReference
]

{ #category : 'accessing' }
FilePosixError >> sourceReference: aFileReference [
	sourceReference := aFileReference
]
