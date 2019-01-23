"
This is the error group for calls to update the file access and modification times.
"
Class {
	#name : 'TouchFileErrorGroup',
	#superclass : 'PosixErrorGroup',
	#category : 'FileSystem-Disk-Options'
}

{ #category : 'constants' }
TouchFileErrorGroup class >> posixErrorNames [
	^#('EACCES' 'ENFILE' 'ENOENT' 'EROFS'  'EIO')
]

{ #category : 'api' }
TouchFileErrorGroup >> errorClassForWindowsPlatformError: anErrorNumber fileReference: aFileReference [

	(#(2 3 161) includes: anErrorNumber) ifTrue: [^self posixErrorNamed: 'ENOENT'].
	anErrorNumber = 4 ifTrue: [^self posixErrorNamed: 'ENFILE'].
	(#(5 123) includes: anErrorNumber) ifTrue: [^self posixErrorNamed: 'EACCES'].
	anErrorNumber = 19 ifTrue: [^self posixErrorNamed: 'EROFS'].
	^self ioFileErrorClass
]
