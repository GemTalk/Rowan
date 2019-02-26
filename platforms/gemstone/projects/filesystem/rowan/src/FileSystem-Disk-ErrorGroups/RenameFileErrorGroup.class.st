Class {
	#name : 'RenameFileErrorGroup',
	#superclass : 'PosixErrorGroup',
	#category : 'FileSystem-Disk-ErrorGroups'
}

{ #category : 'constants' }
RenameFileErrorGroup class >> posixErrorNames [
	^#('EACCES' 'EBUSY' 'EEXIST' 'EINVAL' 'ENOENT' 'ENOSPC' 'EROFS' 'EXDEV'  'EIO')
]

{ #category : 'errors - windows' }
RenameFileErrorGroup >> errorClassForWindowsPlatformError: anErrorNumber newFileReference: aNewFileReference oldFileReference: anOldFileReference [

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
	anErrorNumber = 183 ifTrue: [ ^self posixErrorNamed: 'EEXIST' ].
	^self ioFileErrorClass
]
