"
Set the file status flags to the value specified by arg.

File access mode (O_RDONLY, O_WRONLY, O_RDWR) and file creation flags (i.e., O_CREAT, O_EXCL, O_NOCTTY, O_TRUNC) in arg are ignored.

On Linux this command can change only the O_APPEND, O_ASYNC, O_DIRECT, O_NOATIME, and O_NONBLOCK flags.

https://linux.die.net/man/2/fcntl
"
Class {
	#name : 'FileSetStatusFlag',
	#superclass : 'FileControlFlag',
	#category : 'FileSystem-Base-FileOptions'
}

{ #category : 'accessing' }
FileSetStatusFlag class >> defaultPlatformValue [
	^4
]

{ #category : 'accessing' }
FileSetStatusFlag class >> posixName [
	^'F_SETFL'
]
