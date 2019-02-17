Class {
	#name : 'PosixErrorGroupClientTest',
	#superclass : 'PosixErrorGroupTest',
	#category : 'FileSystem-Tests-Disk-Base'
}

{ #category : 'private' }
PosixErrorGroupClientTest >> setupFileSystem [
	fileSystem := FileSystem clientDisk
]

{ #category : 'tests' }
PosixErrorGroupClientTest >> testDelegationOfFileOpenError [
	| aClass  aFilePosixError|
	fileReference := fileSystem workingDirectory / 't1'.
	self assert: fileSystem store class name equals: #ClientStore.

	fileSystem store store class name = #WindowsStore
		ifTrue: [
			aClass := OpenFileErrorGroup new errorClassForErrorNumber: 19 fileReference: fileReference options: FileOptions read.
			self assert: aClass posixName equals: 'EROFS'.

			aFilePosixError := OpenFileErrorGroup new errorForNumber: 19 fileReference: fileReference options: FileOptions read.
			self assert: aFilePosixError class posixName equals: 'EROFS'.

		posixErrorClass := ChangeModeErrorGroup new errorClassForErrorNumber: 123 fileReference: fileReference.
		self assert: posixErrorClass posixName equals: 'EACCES'].
]
