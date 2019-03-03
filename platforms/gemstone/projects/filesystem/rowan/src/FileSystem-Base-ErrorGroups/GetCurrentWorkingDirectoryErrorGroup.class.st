Class {
	#name : 'GetCurrentWorkingDirectoryErrorGroup',
	#superclass : 'PosixErrorGroup',
	#category : 'FileSystem-Base-ErrorGroups'
}

{ #category : 'errors - windows' }
GetCurrentWorkingDirectoryErrorGroup >> errorClassForWindowsPlatformError: anErrorNumber fileReference: aFileReference [

	^self ioFileErrorClass
]
