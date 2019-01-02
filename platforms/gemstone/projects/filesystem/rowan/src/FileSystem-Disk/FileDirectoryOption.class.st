"
Return an error (errno equals ENOTDIR) if pathname is not a directory. This flag is an extension designed specifically for implementing opendir(). The _GNU_SOURCE feature test macro must be defined in order to make this constant definition available from <fcntl.h>.
"
Class {
	#name : 'FileDirectoryOption',
	#superclass : 'FileCreationOption',
	#category : 'FileSystem-Disk-Options'
}

{ #category : 'accessing' }
FileDirectoryOption class >> posixName [
	^'O_DIRECTORY'
]

{ #category : 'accessing' }
FileDirectoryOption class >> posixValue [
	self error: 'Not supported'
]
