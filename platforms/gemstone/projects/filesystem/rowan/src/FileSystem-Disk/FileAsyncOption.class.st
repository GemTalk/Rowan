"
Generate a signal when I/O becomes possible on the file descriptor returned by open(). This feature, termed signal-driven I/O, is available only for certain file types, such as terminals, FIFOs, and sockets. (The O_ASYNC flag is not specified in SUSv3; however, it, or the older synonym, FASYNC, is found on most UNIX implementations.) 

On Linux, specifying the O_ASYNC flag when calling open() has no effect. To enable signal-driven I/O, we must instead set this flag using the fcntl() F_SETFL operation. (Several other UNIX implementations behave similarly.)
"
Class {
	#name : 'FileAsyncOption',
	#superclass : 'FileStatusOption',
	#category : 'FileSystem-Disk-Options'
}

{ #category : 'accessing' }
FileAsyncOption class >> posixName [
	^'O_ASYNC'
]
