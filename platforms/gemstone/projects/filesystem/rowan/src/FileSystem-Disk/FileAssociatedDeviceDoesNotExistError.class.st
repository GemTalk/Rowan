"
The named file is a character special or block special file, and the device associated with this special file does not exist.
"
Class {
	#name : 'FileAssociatedDeviceDoesNotExistError',
	#superclass : 'FilePosixError',
	#category : 'FileSystem-Disk-Errors'
}

{ #category : 'posixName' }
FileAssociatedDeviceDoesNotExistError class >> errorNumber [
	^10006
]

{ #category : 'posixName' }
FileAssociatedDeviceDoesNotExistError class >> posixName [
	^'ENXIO'
]
