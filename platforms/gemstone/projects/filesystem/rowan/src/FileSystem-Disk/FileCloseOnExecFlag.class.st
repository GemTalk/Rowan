"
Enable the close-on-exec flag for the new file descriptor.
Specifying this flag permits a program to avoid additional
fcntl(2) F_SETFD operations to set the FD_CLOEXEC flag.

Note that the use of this flag is essential in some
multithreaded programs, because using a separate fcntl(2)
F_SETFD operation to set the FD_CLOEXEC flag does not suffice
to avoid race conditions where one thread opens a file
descriptor and attempts to set its close-on-exec flag using
fcntl(2) at the same time as another thread does a fork(2)
plus execve(2).  Depending on the order of execution, the race
may lead to the file descriptor returned by open() being
unintentionally leaked to the program executed by the child
process created by fork(2).  (This kind of race is in
principle possible for any system call that creates a file
descriptor whose close-on-exec flag should be set, and various
other Linux system calls provide an equivalent of the
O_CLOEXEC flag to deal with this problem.)
"
Class {
	#name : 'FileCloseOnExecFlag',
	#superclass : 'FileOpeningFlag',
	#category : 'FileSystem-Disk-Options'
}

{ #category : 'accessing' }
FileCloseOnExecFlag class >> posixName [
	^'O_CLOEXEC'
]
