Class {
	#name : 'CloseDirectoryErrorGroup',
	#superclass : 'PosixErrorGroup',
	#category : 'FileSystem-Disk-Options'
}

{ #category : 'constants' }
CloseDirectoryErrorGroup class >> posixErrorNames [
	^#('EBADF'  'EIO')
]

{ #category : 'errors - windows' }
CloseDirectoryErrorGroup >> errorClassForWindowsPlatformError: anErrorNumber fileReference: aFileReference [

	anErrorNumber = 6 ifTrue: [^self posixErrorNamed: 'EBADF'].
	^self ioFileErrorClass
]
