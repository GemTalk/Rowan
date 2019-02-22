"
O_DIRECTORY

Return an error (errno equals ENOTDIR) if pathname is not a directory. 
This flag is an extension designed specifically for implementing opendir(). 
The _GNU_SOURCE feature test macro must be defined in order to make this constant definition available from <fcntl.h>.

If path resolves to a non-directory file, fail and set errno to [ENOTDIR].
"
Class {
	#name : 'FileDirectoryFlag',
	#superclass : 'FileOpeningFlag',
	#category : 'FileSystem-Disk-Options'
}

{ #category : 'accessing' }
FileDirectoryFlag class >> posixName [
	^'O_DIRECTORY'
]
