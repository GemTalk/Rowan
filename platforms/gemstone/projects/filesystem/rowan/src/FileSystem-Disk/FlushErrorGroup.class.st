Class {
	#name : 'FlushErrorGroup',
	#superclass : 'PosixErrorGroup',
	#category : 'FileSystem-Disk-Options'
}

{ #category : 'constants' }
FlushErrorGroup class >> posixErrorNames [
	^#('EACCES' 'EBADF'  'EIO')
]

{ #category : 'errors - windows' }
FlushErrorGroup >> errorClassForWindowsPlatformError: anErrorNumber fileReference: aFileReference [

	anErrorNumber = 5 ifTrue: [^self posixErrorNamed: 'EACCES'].
	anErrorNumber = 6 ifTrue: [^self posixErrorNamed: 'EBADF'].
	^self ioFileErrorClass
]
