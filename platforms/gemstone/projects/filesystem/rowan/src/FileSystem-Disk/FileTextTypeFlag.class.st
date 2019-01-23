"
Used to indicate a exclusive (write) ADVISORY lock when the file is opened.

Take out a write lease. This will cause the caller to be notified when the file is opened for reading or writing or is truncated. A write lease may be placed on a file only if there are no other open file descriptors for the file. 

Linux supports this type of lock.
"
Class {
	#name : 'FileTextTypeFlag',
	#superclass : 'FileTypeFlag',
	#category : 'FileSystem-Disk-Options'
}

{ #category : 'accessing' }
FileTextTypeFlag class >> defaultPlatformValue [
	^0
]

{ #category : 'accessing' }
FileTextTypeFlag class >> posixName [
	^'O_TEXT'
]
