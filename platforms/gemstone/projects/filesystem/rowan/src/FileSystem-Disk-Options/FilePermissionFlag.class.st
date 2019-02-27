"
The subclasses of this class define POSIX style permission options.

For Windows, there are only two applicable options: Read and Write.
So one only needs to use FileOwnerReadPermissionFlag, FileOwnerWritePermissionFlag, or FileOwnerAllPermissionFlag with Windows files.
All other PermissionFlags are ignored.
"
Class {
	#name : 'FilePermissionFlag',
	#superclass : 'PosixFlag',
	#category : 'FileSystem-Disk-Options'
}
