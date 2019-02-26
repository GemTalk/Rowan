Class {
	#name : 'ChangeModeErrorGroup',
	#superclass : 'PosixErrorGroup',
	#category : 'FileSystem-Disk-ErrorGroups'
}

{ #category : 'constants' }
ChangeModeErrorGroup class >> posixErrorNames [
	^#('EACCES' 'ENOENT' 'EPERM' 'EROFS'  'EIO')
]

{ #category : 'errors - windows' }
ChangeModeErrorGroup >> errorClassForWindowsPlatformError: anErrorNumber fileReference: aFileReference [
	"#isInUse - needs to be implemented - ^(CfsStat stat: path) isCfsError not - 
		Answer true iff the name specified by path is in use (i.e. it is a file or directory). 
		Otherwise, answer false."
	(anErrorNumber = 2 and:[aFileReference exists not]) ifTrue: [^self posixErrorNamed: 'ENOENT'].
	anErrorNumber = 123 ifTrue: [^self posixErrorNamed: 'EACCES'].
	anErrorNumber = 19 ifTrue: [^self posixErrorNamed: 'EROFS'].
	^self ioFileErrorClass
]
