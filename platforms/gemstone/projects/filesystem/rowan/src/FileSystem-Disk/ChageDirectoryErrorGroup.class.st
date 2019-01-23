Class {
	#name : 'ChageDirectoryErrorGroup',
	#superclass : 'PosixErrorGroup',
	#category : 'FileSystem-Disk-Options'
}

{ #category : 'constants' }
ChageDirectoryErrorGroup class >> posixErrorNames [
	^#('EACCES' 'ENOENT' 'EIO')
]

{ #category : 'errors - windows' }
ChageDirectoryErrorGroup >> errorClassForWindowsPlatformError: anErrorNumber fileReference: aFileReference [

	((#(161 2 3 5) includes: anErrorNumber) and: [aFileReference isDirectory not]) 
		ifTrue: [^self posixErrorNamed: 'ENOENT'].
	((#(0 3 123 267 87) includes: anErrorNumber) and: [aFileReference isDirectory]) 
		ifTrue: [^self posixErrorNamed: 'EACCES'].
	^self ioFileErrorClass
]
