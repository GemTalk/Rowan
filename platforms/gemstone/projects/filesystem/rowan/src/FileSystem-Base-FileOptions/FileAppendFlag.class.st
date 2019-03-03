"
The following is from http://man7.org/linux/man-pages/man2/open.2.html

O_APPEND
              The file is opened in append mode.  Before each write(2), the
              file offset is positioned at the end of the file, as if with
              lseek(2).  The modification of the file offset and the write
              operation are performed as a single atomic step.

              O_APPEND may lead to corrupted files on NFS filesystems if
              more than one process appends data to a file at once.  This is
              because NFS does not support appending to a file, so the
              client kernel has to simulate it, which can't be done without
              a race condition.
"
Class {
	#name : 'FileAppendFlag',
	#superclass : 'FileOpeningFlag',
	#category : 'FileSystem-Base-FileOptions'
}

{ #category : 'accessing' }
FileAppendFlag class >> defaultPlatformValue [
	^16r00400
]

{ #category : 'accessing' }
FileAppendFlag class >> posixName [
	^'O_APPEND'
]

{ #category : 'accessing' }
FileAppendFlag class >> windowsValue [
	^16r10000
]
