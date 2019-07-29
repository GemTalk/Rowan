Class {
	#name : 'ReadDirectoryErrorGroup',
	#superclass : 'PosixErrorGroup',
	#category : 'FileSystem-Base-ErrorGroups'
}

{ #category : 'constants' }
ReadDirectoryErrorGroup class >> posixErrorNames [
	^#('EACCES' 'EBADF'  'EIO')
]

{ #category : 'api' }
ReadDirectoryErrorGroup >> errorClassForWindowsPlatformError: anErrorNumber fileReference: aFileReference [

	anErrorNumber = 3 ifTrue: [^self posixErrorNamed: 'EACCES'].
	anErrorNumber = 7 ifTrue: [^self posixErrorNamed: 'EINVAL'].
	^self ioFileErrorClass
]
