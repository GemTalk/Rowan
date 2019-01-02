"
Dont dereference symbolic links.

Normally, open() dereferences pathname if it is a symbolic link. However, if the O_NOFOLLOW flag is specified, then open() fails (with errno set to ELOOP) if pathname is a symbolic link. This flag is useful, especially in privileged programs, for ensuring that open() doesnt dereference a symbolic link. To expose the definition of this flag from <fcntl.h>, we must define the _GNU_SOURCE feature test macro.
"
Class {
	#name : 'FileKeepSymbolicLinksOption',
	#superclass : 'FileCreationOption',
	#category : 'FileSystem-Disk-Options'
}

{ #category : 'accessing' }
FileKeepSymbolicLinksOption class >> posixName [
	^'O_NOFOLLOW'
]

{ #category : 'accessing' }
FileKeepSymbolicLinksOption class >> posixValue [
	self error: 'Not supported'
]
