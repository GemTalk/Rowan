Class {
	#name : 'CloseFileErrorGroup',
	#superclass : 'PosixErrorGroup',
	#category : 'FileSystem-Disk-ErrorGroups'
}

{ #category : 'constants' }
CloseFileErrorGroup class >> posixErrorNames [
	^#('EBADF'  'EIO')
]

{ #category : 'errors - windows' }
CloseFileErrorGroup >> errorClassForWindowsPlatformError: anErrorNumber fileReference: aFileReference [

	anErrorNumber = 6 ifTrue: [^self posixErrorNamed: 'EBADF'].
	^self ioFileErrorClass
]
