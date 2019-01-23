Class {
	#name : 'FileSizeErrorGroup',
	#superclass : 'PosixErrorGroup',
	#category : 'FileSystem-Disk-Options'
}

{ #category : 'constants' }
FileSizeErrorGroup class >> posixErrorNames [
	^#('EBADF'  'EIO')
]

{ #category : 'api' }
FileSizeErrorGroup >> errorClassForWindowsPlatformError: anErrorNumber fileReference: aFileReference [

	anErrorNumber = 6 ifTrue: [^self posixErrorNamed: 'EBADF'].
	^self ioFileErrorClass
]
