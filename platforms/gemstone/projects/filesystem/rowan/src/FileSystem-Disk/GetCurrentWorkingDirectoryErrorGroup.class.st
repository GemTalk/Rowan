Class {
	#name : 'GetCurrentWorkingDirectoryErrorGroup',
	#superclass : 'PosixErrorGroup',
	#category : 'FileSystem-Disk-Options'
}

{ #category : 'errors - windows' }
GetCurrentWorkingDirectoryErrorGroup >> errorClassForWindowsPlatformError: anErrorNumber fileReference: aFileReference [

	^self ioFileErrorClass
]
