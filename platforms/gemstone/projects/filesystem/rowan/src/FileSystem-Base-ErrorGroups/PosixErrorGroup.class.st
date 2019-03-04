Class {
	#name : 'PosixErrorGroup',
	#superclass : 'Object',
	#instVars : [
		'errorClasses'
	],
	#category : 'FileSystem-Base-ErrorGroups'
}

{ #category : 'constants' }
PosixErrorGroup class >> ioFileErrorClass [
	^FileIOError
]

{ #category : 'instance creation' }
PosixErrorGroup class >> new [
	^self basicNew
		initialize;
		yourself
]

{ #category : 'constants' }
PosixErrorGroup class >> posixErrorNames [
	^#()
]

{ #category : 'accessing' }
PosixErrorGroup >> errorClasses [
	^errorClasses
]

{ #category : 'accessing' }
PosixErrorGroup >> errorClasses: aCollection [
	errorClasses := aCollection
]

{ #category : 'errors - default' }
PosixErrorGroup >> errorClassForDefaultPlatformError: anErrorNumber fileReference: aFileReference [

	^self errorClassForErrorNumber: anErrorNumber
]

{ #category : 'errors - default' }
PosixErrorGroup >> errorClassForDefaultPlatformError: anErrorNumber fileReference: aFileReference options: aFileOptions [

	^self errorClassForErrorNumber: anErrorNumber
]

{ #category : 'errors - default' }
PosixErrorGroup >> errorClassForDefaultPlatformError: anErrorNumber newFileReference: aNewFileReference oldFileReference: anOldFileReference [

	^self errorClassForErrorNumber: anErrorNumber
]

{ #category : 'private' }
PosixErrorGroup >> errorClassForErrorNumber: anErrorNumber [
	^self errorClasses detect: [:ea | ea defaultErrorNumber = anErrorNumber] ifNone: [self error: 'Unexpected Posix Error was discoverd']
]

{ #category : 'api' }
PosixErrorGroup >> errorClassForErrorNumber: anErrorNumber fileReference: aFileReference [

	^aFileReference store errorClassForErrorNumber: anErrorNumber fileReference: aFileReference errorGroup: self
]

{ #category : 'api' }
PosixErrorGroup >> errorClassForErrorNumber: anErrorNumber fileReference: aFileReference options: aFileOptions [

	^aFileReference store errorClassForErrorNumber: anErrorNumber fileReference: aFileReference  options: aFileOptions errorGroup: self
]

{ #category : 'api' }
PosixErrorGroup >> errorClassForErrorNumber: anErrorNumber newFileReference: aNewFileReference oldFileReference: anOldFileReference [

	^aNewFileReference store errorClassForErrorNumber: anErrorNumber newFileReference: aNewFileReference oldFileReference: anOldFileReference errorGroup: self
]

{ #category : 'errors - windows' }
PosixErrorGroup >> errorClassForWindowsPlatformError: anErrorNumber fileReference: aFileReference [

	self subclassResponsibility
]

{ #category : 'errors - windows' }
PosixErrorGroup >> errorClassForWindowsPlatformError: anErrorNumber fileReference: aFileReference options: fileOptions [

	self subclassResponsibility
]

{ #category : 'errors - windows' }
PosixErrorGroup >> errorClassForWindowsPlatformError: anErrorNumber newFileReference: aNewFileReference oldFileReference: anOldFileReference [

	self subclassResponsibility
]

{ #category : 'private' }
PosixErrorGroup >> errorClassWithPosixName: aPosixName [
	^self errorClasses detect: [:ea | ea posixName = aPosixName] ifNone: [nil]
]

{ #category : 'api' }
PosixErrorGroup >> errorForNumber: anErrorNumber fileReference: aFileReference [
	| aFilePosixErrorClass |
	aFilePosixErrorClass := self errorClassForErrorNumber: anErrorNumber fileReference: aFileReference.
	^(aFilePosixErrorClass reference: aFileReference)
		platformErrorNumber: anErrorNumber;
		errorGroupName: self class name asString;
		yourself
]

{ #category : 'api' }
PosixErrorGroup >> errorForNumber: anErrorNumber fileReference: aFileReference options: aFileOptions [
	| aFilePosixErrorClass |
	aFilePosixErrorClass := self errorClassForErrorNumber: anErrorNumber fileReference: aFileReference options: aFileOptions.
	^(aFilePosixErrorClass reference: aFileReference)
		platformErrorNumber: anErrorNumber;
		errorGroupName: self class name asString;
		options: aFileOptions
]

{ #category : 'api' }
PosixErrorGroup >> errorForNumber: anErrorNumber newFileReference: aNewFileReference oldFileReference: anOldFileReference [
	| aFilePosixErrorClass |
	aFilePosixErrorClass := self errorClassForErrorNumber: anErrorNumber newFileReference: aNewFileReference oldFileReference: anOldFileReference.
	^(aFilePosixErrorClass reference: aNewFileReference)
		platformErrorNumber: anErrorNumber;
		sourceReference: anOldFileReference;
		errorGroupName: self class name asString;
		yourself
]

{ #category : 'initialization' }
PosixErrorGroup >> initialize [
	self initializeErrorClasses
]

{ #category : 'initialization' }
PosixErrorGroup >> initializeErrorClasses [
	errorClasses := OrderedCollection new.
	self class posixErrorNames do: [:ea | self errorClasses add: (FilePosixError classWithPosixName: ea)]
]

{ #category : 'constants' }
PosixErrorGroup >> ioFileErrorClass [
	^self class ioFileErrorClass
]

{ #category : 'private' }
PosixErrorGroup >> posixErrorNamed: aPosixName [
	^self errorClassWithPosixName: aPosixName
]

{ #category : 'constants' }
PosixErrorGroup >> posixErrorNames [
	^self class posixErrorNames
]

{ #category : 'initialization' }
PosixErrorGroup >> selector: aSelector errorNames: posixErrorNames [

	self selector: aSelector.
	posixErrorNames collect: [:ea | self errorClasses add: (FilePosixError classWithPosixName: ea)]
]

{ #category : 'api' }
PosixErrorGroup >> signalErrorNumber: anErrorNumber fileReference: aFileReference [
	(self errorForNumber: anErrorNumber fileReference: aFileReference) signal
]

{ #category : 'api' }
PosixErrorGroup >> signalErrorNumber: anErrorNumber fileReference: aFileReference options: aFileOptions [
	(self errorForNumber: anErrorNumber fileReference: aFileReference options: aFileOptions) signal
]

{ #category : 'api' }
PosixErrorGroup >> signalErrorNumber: anErrorNumber newFileReference: aNewFileReference oldFileReference: anOldFileReference [
	(self errorForNumber: anErrorNumber  newFileReference: aNewFileReference oldFileReference: anOldFileReference) signal
]
