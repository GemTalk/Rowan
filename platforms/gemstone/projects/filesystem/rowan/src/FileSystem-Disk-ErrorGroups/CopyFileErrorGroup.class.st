"
POSIX defines valid errors for each file/directory action - ex: Read, Write, Open, etc.

THis is the abstract superclass whose subclasses model the valid error messages for each file/directory action which produce error messages.

The class method #posixNames needs to be defined with posix names of the errors supported by the action.
"
Class {
	#name : 'CopyFileErrorGroup',
	#superclass : 'PosixErrorGroup',
	#category : 'FileSystem-Disk-ErrorGroups'
}

{ #category : 'constants' }
CopyFileErrorGroup class >> posixErrorNames [
	^#('EACCES' 'EBUSY' 'EEXIST' 'EIO' 'EISDIR' 'ENOENT' 'EROFS'  'EIO')
]

{ #category : 'errors - windows' }
CopyFileErrorGroup >> errorClassForWindowsPlatformError: anErrorNumber newFileReference: aNewFileReference oldFileReference: anOldFileReference [

	(#(3 21) includes: anErrorNumber) ifTrue: [^self posixErrorNamed: 'EACCES'].
	anErrorNumber = 5 ifTrue: [
		aNewFileReference isDirectory ifTrue: [^self posixErrorNamed: 'EEXIST'].
		aNewFileReference isFile ifTrue: [^self posixErrorNamed: 'EEXIST'].
		anOldFileReference exists ifFalse: [^self posixErrorNamed: 'ENOENT'].
		anOldFileReference isFile
			ifTrue: [^self posixErrorNamed: 'EACCES']
			ifFalse: [^self posixErrorNamed: 'EISDIR']. ].
	(#(16 32 33) includes: anErrorNumber) ifTrue: [^self posixErrorNamed: 'EBUSY'].
	anErrorNumber = 19 ifTrue: [^self posixErrorNamed: 'EROFS'].
	anErrorNumber = 80 ifTrue: [^self posixErrorNamed: 'EEXIST'].
	anErrorNumber = 87 ifTrue: [
		^anOldFileReference exists not
			ifTrue: [self posixErrorNamed: 'ENOENT']
			ifFalse: [self posixErrorNamed: 'EACCES']].
	anErrorNumber = 123 ifTrue: [^self posixErrorNamed: 'EACCES'].
	anErrorNumber = 161 ifTrue: [^self posixErrorNamed: 'ENOENT'].
	anErrorNumber = 183 ifTrue: [^self posixErrorNamed: 'EEXIST'].
	^self ioFileErrorClass
]
