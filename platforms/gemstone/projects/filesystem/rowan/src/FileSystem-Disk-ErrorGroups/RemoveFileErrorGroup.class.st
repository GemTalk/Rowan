Class {
	#name : 'RemoveFileErrorGroup',
	#superclass : 'PosixErrorGroup',
	#category : 'FileSystem-Disk-ErrorGroups'
}

{ #category : 'constants' }
RemoveFileErrorGroup class >> posixErrorNames [
	^#('EACCES' 'EBUSY' 'EEXIST' 'EINVAL' 'EISDIR' 'ENOENT' 'EPERM' 'EROFS'  'EIO')
]

{ #category : 'errors - windows' }
RemoveFileErrorGroup >> errorClassForWindowsPlatformError: anErrorNumber fileReference: aFileReference [

	anErrorNumber = 2 ifTrue: [
		aFileReference isDirectory 
			ifTrue: [^self posixErrorNamed: 'EISDIR']
			ifFalse: [^self posixErrorNamed: 'ENOENT']].
	anErrorNumber = 3 ifTrue: [^self posixErrorNamed: 'EACCES'].
	anErrorNumber = 5 ifTrue: [
		aFileReference isDirectory 
			ifTrue: [^self posixErrorNamed: 'EISDIR']
			ifFalse: [^self posixErrorNamed: 'EACCES']].
	anErrorNumber = 19 ifTrue: [^self posixErrorNamed: 'EROFS'].
	anErrorNumber = 123 ifTrue: [^self posixErrorNamed: 'EACCES'].
	anErrorNumber = 145 ifTrue: [^self posixErrorNamed: 'EEXIST'].
	anErrorNumber = 161 ifTrue: [^self posixErrorNamed: 'ENOENT'].
	^self ioFileErrorClass
]
