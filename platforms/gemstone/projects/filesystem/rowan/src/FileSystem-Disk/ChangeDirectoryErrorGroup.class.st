Class {
	#name : 'ChangeDirectoryErrorGroup',
	#superclass : 'PosixErrorGroup',
	#category : 'FileSystem-Disk-Options'
}

{ #category : 'constants' }
ChangeDirectoryErrorGroup class >> posixErrorNames [
	^#('EACCES' 'ENOENT' 'EIO')
]

{ #category : 'errors - windows' }
ChangeDirectoryErrorGroup >> errorClassForWindowsPlatformError: anErrorNumber fileReference: aFileReference [

	((#(161 2 3 5) includes: anErrorNumber) and: [aFileReference isDirectory not]) 
		ifTrue: [^self posixErrorNamed: 'ENOENT'].
	((#(0 3 123 267 87) includes: anErrorNumber) and: [aFileReference isDirectory]) 
		ifTrue: [^self posixErrorNamed: 'EACCES'].
	^self ioFileErrorClass
]

{ #category : 'constants' }
ChangeDirectoryErrorGroup >> posixErrorNames [
	^#('EACCES' 'ENOENT' 'EIO')
]
