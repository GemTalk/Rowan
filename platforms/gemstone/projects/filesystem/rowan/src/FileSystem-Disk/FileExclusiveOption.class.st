"
This flag is used in conjunction with O_CREAT to indicate that if the file already exists, it should not be opened; instead, open() should fail, with errno set to EEXIST. In other words, this flag allows the caller to ensure that it is the process creating the file. The check for existence and the creation of the file are performed atomically. When both O_CREAT and O_EXCL are specified in flags, open() fails (with the error EEXIST) if pathname is a symbolic link. SUSv3 requires this behavior so that a privileged application can create a file in a known location without there being a possibility that a symbolic link would cause the file to be created in a different location (e.g., a system directory), which would have security implications.
"
Class {
	#name : 'FileExclusiveOption',
	#superclass : 'FileCreationOption',
	#category : 'FileSystem-Disk-Options'
}

{ #category : 'accessing' }
FileExclusiveOption class >> posixName [
	^'O_EXCL'
]

{ #category : 'accessing' }
FileExclusiveOption class >> posixValue [
	^16r40000
]
