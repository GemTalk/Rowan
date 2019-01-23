"
Try to minimize cache effects of the I/O to and from this file.  In general this will degrade performance, but it is useful in special situations, such as when applications do their own caching.  File I/O is done directly to/from user-space buffers.  The O_DIRECT flag on its own makes an effort to transfer data synchronously, but does not give the guarantees of the O_SYNC flag that data and necessary metadata are transferred.  To guarantee synchronous I/O, O_SYNC must be used in addition to O_DIRECT.

====

Allow file I/O to bypass the buffer cache. The _GNU_SOURCE feature test macro must be defined in order to make this constant definition available from <fcntl.h>.

O_DIRECT alone only promises that the kernel will avoid copying data from user space to kernel space, and will instead write it directly via DMA (Direct memory access; if possible). Data does not go into caches. There is no strict guarantee that the function will return only after all data has been transferred.

O_SYNC guarantees that the call will not return before all data has been transferred to the disk (as far as the OS can tell). This still does not guarantee that the data isn't somewhere in the harddisk write cache, but it is as much as the OS can guarantee.

O_DIRECT|O_SYNC is the combination of these, i.e. ""DMA + guarantee"".
"
Class {
	#name : 'FileDirectFlag',
	#superclass : 'FileOpeningFlag',
	#category : 'FileSystem-Disk-Options'
}

{ #category : 'accessing' }
FileDirectFlag class >> posixName [
	^'O_DIRECT'
]
