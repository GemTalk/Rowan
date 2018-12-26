"
Set the close-on-exec flag (since Linux 2.6.23) - SUSv4

Enable the close-on-exec flag (FD_CLOEXEC) for the new file descriptor. Using the O_CLOEXEC flag allows a program to avoid additional fcntl() F_SETFD and F_SETFD operations to set the close-on-exec flag. It is also necessary in multithreaded programs to avoid the race conditions that could occur using the latter technique. These races can occur when one thread opens a file descriptor and then tries to mark it close-on-exec at the same time as another thread does a fork() and then an exec() of an arbitrary program. (Suppose that the second thread manages to both fork() and exec() between the time the first thread opens the file descriptor and uses fcntl() to set the close-on-exec flag.) Such races could result in open file descriptors being unintentionally passed to unsafe programs.
"
Class {
	#name : 'FileCloseOnExecOption',
	#superclass : 'FileCreationOption',
	#category : 'FileSystem-Disk-Options'
}

{ #category : 'accessing' }
FileCloseOnExecOption class >> posixName [
	^'O_CLOEXEC'
]
