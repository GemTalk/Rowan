Class {
	#name : 'RenameErrorGroup',
	#superclass : 'PosixErrorGroup',
	#category : 'FileSystem-Disk-Options'
}

{ #category : 'constants' }
RenameErrorGroup class >> posixErrorNames [
	^#('EACCES' 'EBUSY' 'EEXIST' 'EINVAL' 'ENOENT' 'ENOSPC' 'EROFS' 'EXDEV'  'EIO')
]

{ #category : 'errors - windows' }
RenameErrorGroup >> errorClassForWindowsPlatformError: anErrorNumber newFileReference: aNewFileReference oldFileReference: anOldFileReference [

	anErrorNumber = 2 ifTrue: [
		aNewFileReference isDirectory 
			ifTrue: [ ^self posixErrorNamed: 'EISDIR' ]
			ifFalse: [ ^self posixErrorNamed: 'ENOENT' ] ].
	anErrorNumber = 5 ifTrue: [
		aNewFileReference isDirectory 
			ifTrue: [ ^self posixErrorNamed: 'EISDIR' ]
			ifFalse: [ ^self posixErrorNamed: 'EACCES' ] ].
	( #( 3 123 ) includes: anErrorNumber ) ifTrue: [ ^self posixErrorNamed: 'EACCES' ].
	anErrorNumber =  19 ifTrue: [ ^self posixErrorNamed: 'EROFS' ].
	anErrorNumber = 145 ifTrue: [ ^self posixErrorNamed: 'EEXIST' ].
	anErrorNumber = 161 ifTrue: [ ^self posixErrorNamed: 'ENOENT' ].
	^self ioFileErrorClass
]
