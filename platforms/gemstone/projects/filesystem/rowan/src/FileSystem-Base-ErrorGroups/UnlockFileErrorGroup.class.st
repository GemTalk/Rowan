Class {
	#name : 'UnlockFileErrorGroup',
	#superclass : 'PosixErrorGroup',
	#category : 'FileSystem-Base-ErrorGroups'
}

{ #category : 'constants' }
UnlockFileErrorGroup class >> posixErrorNames [
	^#('EACCES' 'EBADF' 'EINVAL' 'EROFS' 'EBUSY'  'EIO')
]

{ #category : 'errors - windows' }
UnlockFileErrorGroup >> errorClassForWindowsPlatformError: anErrorNumber fileReference: aFileReference [

	(#(1 158) includes: anErrorNumber) ifTrue: [^self posixErrorNamed: 'EINVAL'].
	anErrorNumber = 5 ifTrue: [^self posixErrorNamed: 'EACCES'].
	anErrorNumber = 6 ifTrue: [^self posixErrorNamed: 'EBADF'].
	anErrorNumber = 19 ifTrue: [^self posixErrorNamed: 'EROFS'].
	(#(32 33) includes: anErrorNumber) ifTrue: [^self posixErrorNamed: 'EBUSY'].
	^self ioFileErrorClass
]
