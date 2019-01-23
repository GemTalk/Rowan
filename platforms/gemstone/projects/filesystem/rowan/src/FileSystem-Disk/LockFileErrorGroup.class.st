Class {
	#name : 'LockFileErrorGroup',
	#superclass : 'PosixErrorGroup',
	#category : 'FileSystem-Disk-Options'
}

{ #category : 'constants' }
LockFileErrorGroup class >> posixErrorNames [
	^#('EACCES' 'EBADF' 'EBUSY' 'EINVAL' 'EROFS'  'EIO')
]

{ #category : 'errors - windows' }
LockFileErrorGroup >> errorClassForWindowsPlatformError: anErrorNumber fileReference: aFileReference [

	anErrorNumber = 1 ifTrue: [^self posixErrorNamed: 'EINVAL'].
	anErrorNumber = 5 ifTrue: [^self posixErrorNamed: 'EACCES'].
	anErrorNumber = 6 ifTrue: [^self posixErrorNamed: 'EBADF'].
	anErrorNumber = 19 ifTrue: [^self posixErrorNamed: 'EROFS'].
	(#(32 33) includes: anErrorNumber) ifTrue: [^self posixErrorNamed: 'EBUSY'].
	^self ioFileErrorClass
]
