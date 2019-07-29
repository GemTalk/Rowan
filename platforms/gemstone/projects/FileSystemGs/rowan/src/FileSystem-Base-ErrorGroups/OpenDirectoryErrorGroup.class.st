Class {
	#name : 'OpenDirectoryErrorGroup',
	#superclass : 'PosixErrorGroup',
	#category : 'FileSystem-Base-ErrorGroups'
}

{ #category : 'constants' }
OpenDirectoryErrorGroup class >> posixErrorNames [
	^#('EACCES' 'EINVAL' 'ENFILE' 'ENOENT'  'EIO')
]

{ #category : 'errors - windows' }
OpenDirectoryErrorGroup >> errorClassForWindowsPlatformError: anErrorNumber fileReference: aFileReference [

	anErrorNumber = 2 ifTrue: [
		aFileReference exists ifFalse: [^self posixErrorNamed: 'ENOENT'] ].
	(#(3 5 123 267) includes: anErrorNumber) ifTrue: [^self posixErrorNamed: 'EACCES'].
	anErrorNumber = 84 ifTrue: [^self posixErrorNamed: 'ENFILE'].
	anErrorNumber = 87 ifTrue: [^self posixErrorNamed: 'EINVAL'].
	anErrorNumber = 161 ifTrue: [^self posixErrorNamed: 'ENOENT'].

	^self ioFileErrorClass
]
