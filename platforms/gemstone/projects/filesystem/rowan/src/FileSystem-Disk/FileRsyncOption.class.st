"
Read I/O operations on the file descriptor shall complete at the same level of integrity as specified by the O_DSYNC and O_SYNC flags. If both O_DSYNC and O_RSYNC are set in oflag, all I/O operations on the file descriptor shall complete as defined by synchronized I/O data integrity completion. If both O_SYNC and O_RSYNC are set in flags, all I/O operations on the file descriptor shall complete as defined by synchronized I/O file integrity completion.

------

The O_RSYNC flag is specified in conjunction with either O_SYNC or O_DSYNC, and extends the write behaviors of these flags to read operations. Specifying both O_RSYNC and O_DSYNC when opening a file means that all subsequent reads are completed according to the requirements of synchronized I/O data integrity (i.e., prior to performing the read, all pending file writes are completed as though carried out with O_DSYNC). Specifying both O_RSYNC and O_SYNC when opening a file means that all subsequent reads are completed according to the requirements of synchronized I/O file integrity (i.e., prior to performing the read, all pending file writes are completed as though carried out with O_SYNC).

Before kernel 2.6.33, the O_DSYNC and O_RSYNC flags were not implemented on Linux, and the glibc headers defined these constants to be the same as O_SYNC. (This isnt actually correct in the case of O_RSYNC, since O_SYNC doesnt provide any functionality for read operations.)

Starting with kernel 2.6.33, Linux implements O_DSYNC, and an implementation of O_RSYNC is likely to be added in a future kernel release.
"
Class {
	#name : 'FileRsyncOption',
	#superclass : 'FileStatusOption',
	#category : 'FileSystem-Disk-Options'
}

{ #category : 'accessing' }
FileRsyncOption class >> posixName [
	^'O_RSYNC'
]

{ #category : 'accessing' }
FileRsyncOption class >> posixValue [
	^self error: 'Not implmented'
]
