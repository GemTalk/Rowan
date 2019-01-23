Class {
	#name : 'FileSeekErrorGroup',
	#superclass : 'PosixErrorGroup',
	#category : 'FileSystem-Disk-Options'
}

{ #category : 'constants' }
FileSeekErrorGroup class >> posixErrorNames [
	^#('EBADF' 'EINVAL'  'EIO')
]

{ #category : 'errors - windows' }
FileSeekErrorGroup >> errorClassForWindowsPlatformError: anErrorNumber fileReference: aFileReference [

	anErrorNumber = 6 ifTrue: [^self posixErrorNamed: 'EBADF'].
	(#(87 131) includes: anErrorNumber) ifTrue: [^self posixErrorNamed: 'EINVAL'].
	^self ioFileErrorClass
]
