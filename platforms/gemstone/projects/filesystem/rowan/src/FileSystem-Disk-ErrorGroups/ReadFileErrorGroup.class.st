Class {
	#name : 'ReadFileErrorGroup',
	#superclass : 'PosixErrorGroup',
	#category : 'FileSystem-Disk-ErrorGroups'
}

{ #category : 'constants' }
ReadFileErrorGroup class >> posixErrorNames [
	^#('EACCES' 'EBADF' 'EBUSY' 'EINVAL'  'EIO')
]

{ #category : 'errors - windows' }
ReadFileErrorGroup >> errorClassForWindowsPlatformError: anErrorNumber fileReference: aFileReference [

	(#(5 112) includes: anErrorNumber) ifTrue: [^self posixErrorNamed: 'EACCES'].
	anErrorNumber = 6 ifTrue: [^self posixErrorNamed: 'EBADF'].
	anErrorNumber = 7 ifTrue: [^self posixErrorNamed: 'EINVAL'].
	anErrorNumber = 33 ifTrue: [^self posixErrorNamed: 'EBUSY'].
	^self ioFileErrorClass
]
