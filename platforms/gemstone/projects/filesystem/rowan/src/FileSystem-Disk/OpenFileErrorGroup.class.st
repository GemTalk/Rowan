Class {
	#name : 'OpenFileErrorGroup',
	#superclass : 'PosixErrorGroup',
	#category : 'FileSystem-Disk-Options'
}

{ #category : 'constants' }
OpenFileErrorGroup class >> posixErrorNames [
	^#('EACCES' 'EBUSY' 'EEXIST' 'EINVAL' 'ENFILE' 'ENOENT' 'ENOSPC' 'EROFS'  'EIO')
]

{ #category : 'errors - windows' }
OpenFileErrorGroup >> errorClassForWindowsPlatformError: anErrorNumber fileReference: aFileReference options: fileOptions [

	anErrorNumber = 2 ifTrue: [
		aFileReference isDirectory 
			ifTrue: [^self posixErrorNamed: 'EACCES']
			ifFalse: [^self posixErrorNamed: 'ENOENT']].
	(#(3 123 161) includes: anErrorNumber) ifTrue: [^self posixErrorNamed: 'EACCES'].
	anErrorNumber = 4 ifTrue: [^self posixErrorNamed: 'ENFILE'].
	(#(5 87) includes: anErrorNumber) ifTrue: [
		aFileReference isDirectory ifTrue: [^self posixErrorNamed: 'EACCES'].
		fileOptions hasFileTruncateFlag ifTrue: [^self posixErrorNamed: 'EINVAL'].
		fileOptions hasFileCreateFlag ifTrue: [^self posixErrorNamed: 'EACCES'].
		aFileReference isFile ifTrue: [^self posixErrorNamed: 'EEXIST'].
		^self posixErrorNamed: 'EINVAL'].
	anErrorNumber = 19 ifTrue: [^self posixErrorNamed: 'EROFS'].
	(#(32 33) includes: anErrorNumber) ifTrue: [^self posixErrorNamed: 'EBUSY'].
	anErrorNumber = 36 ifTrue: [^self posixErrorNamed: 'ENFILE']. "no more buffers available. Too many in use"
	anErrorNumber = 80   "file exists"
		ifTrue: [
			fileOptions hasExclsiveFlag 
				ifTrue: [^self posixErrorNamed: 'EACCES']
				ifFalse: [^self posixErrorNamed: 'EEXIST'] ].
	^self ioFileErrorClass
]
