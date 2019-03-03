Class {
	#name : 'FlushFileErrorGroup',
	#superclass : 'PosixErrorGroup',
	#category : 'FileSystem-Base-ErrorGroups'
}

{ #category : 'constants' }
FlushFileErrorGroup class >> posixErrorNames [
	^#('EACCES' 'EBADF'  'EIO')
]

{ #category : 'errors - windows' }
FlushFileErrorGroup >> errorClassForWindowsPlatformError: anErrorNumber fileReference: aFileReference [

	anErrorNumber = 5 ifTrue: [^self posixErrorNamed: 'EACCES'].
	anErrorNumber = 6 ifTrue: [^self posixErrorNamed: 'EBADF'].
	^self ioFileErrorClass
]
