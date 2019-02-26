Class {
	#name : 'RemoveDirectoryErrorGroup',
	#superclass : 'PosixErrorGroup',
	#category : 'FileSystem-Disk-ErrorGroups'
}

{ #category : 'constants' }
RemoveDirectoryErrorGroup class >> posixErrorNames [
	^#('EACCES' 'EBUSY' 'EEXIST' 'EINVAL' 'ENOENT' 'EROFS'  'EIO')
]

{ #category : 'errors - windows' }
RemoveDirectoryErrorGroup >> errorClassForWindowsPlatformError: anErrorNumber fileReference: aDirectoryReference [

	anErrorNumber = 2 ifTrue: [^self posixErrorNamed: 'ENOENT'].
	anErrorNumber = 3 ifTrue: [
		aDirectoryReference exists not ifTrue: [^self posixErrorNamed: 'ENOENT'].
		aDirectoryReference entries isEmpty ifTrue: [^self posixErrorNamed: 'EEXIST'].
		^self posixErrorNamed: 'EACCES'].
	anErrorNumber = 5 ifTrue: [
		aDirectoryReference exists not ifTrue: [^self posixErrorNamed: 'ENOENT'].
		aDirectoryReference entries isEmpty ifTrue: [^self posixErrorNamed: 'EEXIST'].
		^self posixErrorNamed: 'EACCES'].
	anErrorNumber = 16 ifTrue: [^self posixErrorNamed: 'EBUSY'].
	anErrorNumber = 19 ifTrue: [^self posixErrorNamed: 'EROFS'].
	anErrorNumber = 123 ifTrue: [^self posixErrorNamed: 'EACCES'].
	anErrorNumber = 145 ifTrue: [^self posixErrorNamed: 'EEXIST'].
	anErrorNumber = 161 ifTrue: [^self posixErrorNamed: 'ENOENT'].
	anErrorNumber = 267 ifTrue: [^self posixErrorNamed: 'ENOENT'].
	^self ioFileErrorClass
]
