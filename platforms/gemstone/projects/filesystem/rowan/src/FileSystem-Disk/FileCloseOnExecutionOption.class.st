"
the close-on-exec flag.  If the FD_CLOEXEC bit is set, the file descriptor will automatically be closed during a successful execve(2)
"
Class {
	#name : 'FileCloseOnExecutionOption',
	#superclass : 'FileControlOption',
	#category : 'FileSystem-Disk-Options'
}

{ #category : 'access' }
FileCloseOnExecutionOption class >> defaultPlatformValue [
	^1
]

{ #category : 'access' }
FileCloseOnExecutionOption class >> posixName [
	^'F_DCLOEXEC'
]
