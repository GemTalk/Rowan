"
This is an abstract class which defines the file modes.
A file mode class is created for each type of mode. These
are subclasses of this class.

The instance returns the mode string to use when opening a file.

Also it knows if the file is read only.
"
Class {
	#name : 'FileMode',
	#superclass : 'Object',
	#instVars : [
		'initialWritePosition',
		'initialReadPosition',
		'truncate',
		'fileSpec'
	],
	#category : 'FileSystem-Core-Kernel'
}

{ #category : 'instance creation' }
FileMode class >> append [
	^#FileAppendMode basicNew
		initializeAppend;
		yourself
]

{ #category : 'instance creation' }
FileMode class >> appendAndRead [
	^#FileAppendMode basicNew
		initializeAppendAndRead;
		yourself
]

{ #category : 'instance creation' }
FileMode class >> read [
	^#FileReadMode basicNew
		initializeRead;
		yourself
]

{ #category : 'instance creation' }
FileMode class >> readWrite [
	^#FileReadWriteMode basicNew
		initializeReadWrite;
		yourself
]

{ #category : 'instance creation' }
FileMode class >> readWriteTruncated [
	^#FileReadWriteMode basicNew
		initializeReadWriteTruncated;
		yourself
]

{ #category : 'instance creation' }
FileMode class >> write [
	^#FileWriteMode basicNew
		initializeWrite;
		yourself
]

{ #category : 'accessing' }
FileMode >> fileSpec [
	^fileSpec
]

{ #category : 'accessing' }
FileMode >> fileSpec: aFileSpec [
	fileSpec := aFileSpec
]

{ #category : 'initialization' }
FileMode >> initialize [
	self truncate: false
]

{ #category : 'initialization' }
FileMode >> initializeAppend [
	self initialize.
	self initialWritePosition: FileEndPosition new.
	self initialReadPosition: FileNotAvailablePosition new
]

{ #category : 'initialization' }
FileMode >> initializeAppendAndRead [
	self initialize.
	self initialWritePosition: FileEndPosition new.
	self initialReadPosition: FileBeginningPosition new
]

{ #category : 'initialization' }
FileMode >> initializeRead [
	self initialize.
	self initialWritePosition: FileNotAvailablePosition new.
	self initialReadPosition: FileBeginningPosition new
]

{ #category : 'initialization' }
FileMode >> initializeReadWrite [
	self initialize.
	self initialWritePosition: FileBeginningPosition new.
	self initialReadPosition: FileBeginningPosition new
]

{ #category : 'initialization' }
FileMode >> initializeReadWriteTruncated [
	self initialize.
	self initialWritePosition: FileBeginningPosition new.
	self initialReadPosition: FileBeginningPosition new.
	self truncate: true
]

{ #category : 'initialization' }
FileMode >> initializeWrite [
	self initialize.
	self initialWritePosition: FileBeginningPosition new.
	self initialReadPosition: FileNotAvailablePosition new.
	self truncate: true
]

{ #category : 'accessing' }
FileMode >> initialReadPosition [
	^initialReadPosition
]

{ #category : 'accessing' }
FileMode >> initialReadPosition: anObject [
	initialReadPosition := anObject
]

{ #category : 'accessing' }
FileMode >> initialWritePosition [
	^initialWritePosition
]

{ #category : 'accessing' }
FileMode >> initialWritePosition: anObject [
	initialWritePosition := anObject
]

{ #category : 'testing' }
FileMode >> isReadable [
	^self initialReadPosition isPositionable
]

{ #category : 'testing' }
FileMode >> isTruncated [
	^self truncate
]

{ #category : 'testing' }
FileMode >> isWritable [
	^self initialWritePosition isPositionable
]

{ #category : 'accessing' }
FileMode >> mode [
	"Return a file's mode string"
	| aStream |
	aStream := WriteStream on: String new.
	self mode printModeOn: aStream.
	^aStream contents
]

{ #category : 'printing' }
FileMode >> printOn: aStream [

	super printOn: aStream.
	aStream 
		nextPutAll: '(';
		nextPutAll: ( self class name == #FileMode ifTrue: ['abstract class'] ifFalse: [ self mode] );
		nextPutAll: ')'
]

{ #category : 'printing' }
FileMode >> printOpenModeStringOn: aStream [

	aStream nextPutAll: self mode
]

{ #category : 'accessing' }
FileMode >> truncate [
	^truncate
]

{ #category : 'accessing' }
FileMode >> truncate: aFileSpec [
	truncate := aFileSpec
]
