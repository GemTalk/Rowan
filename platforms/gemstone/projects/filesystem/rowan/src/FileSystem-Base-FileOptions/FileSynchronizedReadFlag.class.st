"
close-on-exec flag.

if the FD_CLOEXEC bit is 0, the file descriptor will remain open across an execve(2), otherwise it will be closed.
"
Class {
	#name : 'FileSynchronizedReadFlag',
	#superclass : 'FileOpeningFlag',
	#category : 'FileSystem-Base-FileOptions'
}

{ #category : 'accessing' }
FileSynchronizedReadFlag class >> posixName [
	^'O_RSYNC'
]
