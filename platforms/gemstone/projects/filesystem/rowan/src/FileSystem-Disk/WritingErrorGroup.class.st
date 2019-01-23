Class {
	#name : 'WritingErrorGroup',
	#superclass : 'PosixErrorGroup',
	#category : 'FileSystem-Disk-Options'
}

{ #category : 'constants' }
WritingErrorGroup class >> posixErrorNames [
	^#('EACCES' 'EBADF' 'EBUSY' 'EINVAL' 'ENOSPC' 'EROFS'  'EIO')
]

{ #category : 'errors - windows' }
WritingErrorGroup >> errorClassForWindowsPlatformError: anErrorNumber fileReference: aFileReference [

	(#(5 112) includes: anErrorNumber) ifTrue: [^self posixErrorNamed: 'EACCES'].
	anErrorNumber = 6 ifTrue: [^self posixErrorNamed: 'EBADF'].
	anErrorNumber = 19 ifTrue: [^self posixErrorNamed: 'EROFS'].
	anErrorNumber = 33 ifTrue: [^self posixErrorNamed: 'EBUSY'].
	^self ioFileErrorClass
]
