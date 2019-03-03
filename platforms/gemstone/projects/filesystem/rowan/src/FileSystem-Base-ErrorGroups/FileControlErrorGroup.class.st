Class {
	#name : 'FileControlErrorGroup',
	#superclass : 'PosixErrorGroup',
	#category : 'FileSystem-Base-ErrorGroups'
}

{ #category : 'constants' }
FileControlErrorGroup class >> posixErrorNames [
	^#('EACCES' 'EBADF' 'EINVAL'  'EIO')
]

{ #category : 'errors - windows' }
FileControlErrorGroup >> errorClassForWindowsPlatformError: anErrorNumber fileReference: aFileReference [

	anErrorNumber = 5 ifTrue: [ ^self posixErrorNamed: 'EACCES' ].
	anErrorNumber = 6 ifTrue: [ ^self posixErrorNamed: 'EBADF' ].
	anErrorNumber = 7 ifTrue: [ ^self posixErrorNamed: 'EINVAL' ].
	^self ioFileErrorClass
]
