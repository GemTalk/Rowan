"
Dont dereference symbolic links. If path names a symbolic link, fail and set errno to [ELOOP].

Normally, open() dereferences pathname if it is a symbolic link. However, if the O_NOFOLLOW flag is specified, then open() fails (with errno set to ELOOP) if pathname is a symbolic link. This flag is useful, especially in privileged programs, for ensuring that open() doesnt dereference a symbolic link. To expose the definition of this flag from <fcntl.h>, we must define the _GNU_SOURCE feature test macro.
"
Class {
	#name : 'FileKeepSymbolicLinksFlag',
	#superclass : 'FileOpeningFlag',
	#category : 'FileSystem-Disk-Options'
}

{ #category : 'accessing' }
FileKeepSymbolicLinksFlag class >> posixName [
	^'O_NOFOLLOW'
]
