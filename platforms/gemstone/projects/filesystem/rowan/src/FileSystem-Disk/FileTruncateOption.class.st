"
If the file already exists and is a regular file, then truncate it to zero length, destroying any existing data. 

On Linux, truncation occurs whether the file is being opened for reading or writing (in both cases, we must have write permission on the file). SUSv3 leaves the combination of O_RDONLY and O_TRUNC unspecified, but most other UNIX implementations behave in the same way as Linux.
"
Class {
	#name : 'FileTruncateOption',
	#superclass : 'FileCreationOption',
	#category : 'FileSystem-Disk-Options'
}

{ #category : 'accessing' }
FileTruncateOption class >> posixName [
	^'O_TRUNC'
]

{ #category : 'accessing' }
FileTruncateOption class >> posixValue [
	^16r80000
]
