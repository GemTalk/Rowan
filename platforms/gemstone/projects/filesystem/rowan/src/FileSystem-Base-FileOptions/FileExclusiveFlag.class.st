"
The following is from: http://man7.org/linux/man-pages/man2/open.2.html

O_EXCL   Ensure that this call creates the file: if this flag is
              specified in conjunction with O_CREAT, and pathname already
              exists, then open() fails with the error EEXIST.

              When these two flags are specified, symbolic links are not
              followed: if pathname is a symbolic link, then open() fails
              regardless of where the symbolic link points.

              In general, the behavior of O_EXCL is undefined if it is used
              without O_CREAT.  There is one exception: on Linux 2.6 and
              later, O_EXCL can be used without O_CREAT if pathname refers
              to a block device.  If the block device is in use by the
              system (e.g., mounted), open() fails with the error EBUSY.

              On NFS, O_EXCL is supported only when using NFSv3 or later on
              kernel 2.6 or later.  In NFS environments where O_EXCL support
              is not provided, programs that rely on it for performing
              locking tasks will contain a race condition.  Portable
              programs that want to perform atomic file locking using a
              lockfile, and need to avoid reliance on NFS support for
              O_EXCL, can create a unique file on the same filesystem (e.g.,
              incorporating hostname and PID), and use link(2) to make a
              link to the lockfile.  If link(2) returns 0, the lock is
              successful.  Otherwise, use stat(2) on the unique file to
              check if its link count has increased to 2, in which case the
              lock is also successful.
"
Class {
	#name : 'FileExclusiveFlag',
	#superclass : 'FileOpeningFlag',
	#category : 'FileSystem-Base-FileOptions'
}

{ #category : 'accessing' }
FileExclusiveFlag class >> defaultPlatformValue [
	^16r0080
]

{ #category : 'accessing' }
FileExclusiveFlag class >> posixName [
	^'O_EXCL'
]

{ #category : 'accessing' }
FileExclusiveFlag class >> windowsValue [
	^16r40000
]
