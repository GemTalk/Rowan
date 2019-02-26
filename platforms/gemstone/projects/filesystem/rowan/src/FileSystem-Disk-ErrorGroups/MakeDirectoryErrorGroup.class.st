Class {
	#name : 'MakeDirectoryErrorGroup',
	#superclass : 'PosixErrorGroup',
	#category : 'FileSystem-Disk-ErrorGroups'
}

{ #category : 'constants' }
MakeDirectoryErrorGroup class >> posixErrorNames [
	^#('EACCES' 'EEXIST' 'ENOSPC' 'EROFS'  'EIO')
]

{ #category : 'errors - windows' }
MakeDirectoryErrorGroup >> errorClassForWindowsPlatformError: anErrorNumber fileReference: aFileReference [

	anErrorNumber = 3 ifTrue: [
		aFileReference exists 
			ifTrue: [^self posixErrorNamed: 'EEXIST']
			ifFalse: [^self posixErrorNamed: 'EACCES'] ].
	anErrorNumber = 5 ifTrue: [
		aFileReference exists 
			ifTrue: [^self posixErrorNamed: 'EEXIST']
			ifFalse: [^self posixErrorNamed: 'EACCES'] ].
	anErrorNumber = 19 ifTrue: [^self posixErrorNamed: 'EROFS'].
	anErrorNumber = 123 ifTrue: [^self posixErrorNamed: 'EACCES'].
	anErrorNumber = 161 ifTrue: [^self posixErrorNamed: 'EACCES'].
	anErrorNumber = 183 ifTrue: [^self posixErrorNamed: 'EEXIST'].
	^self ioFileErrorClass
]
