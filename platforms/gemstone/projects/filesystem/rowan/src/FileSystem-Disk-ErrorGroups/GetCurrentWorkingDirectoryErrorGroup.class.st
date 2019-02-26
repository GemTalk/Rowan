Class {
	#name : 'GetCurrentWorkingDirectoryErrorGroup',
	#superclass : 'PosixErrorGroup',
	#category : 'FileSystem-Disk-ErrorGroups'
}

{ #category : 'errors - windows' }
GetCurrentWorkingDirectoryErrorGroup >> errorClassForWindowsPlatformError: anErrorNumber fileReference: aFileReference [

	^self ioFileErrorClass
]
