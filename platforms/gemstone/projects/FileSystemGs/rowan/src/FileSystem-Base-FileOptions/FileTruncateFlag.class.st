"
If the file already exists and is a regular file, then truncate it to zero length, destroying any existing data.

On Linux, truncation occurs whether the file is being opened for reading or writing (in both cases, we must have write permission on the file). SUSv3 leaves the combination of O_RDONLY and O_TRUNC unspecified, but most other UNIX implementations behave in the same way as Linux.
"
Class {
	#name : 'FileTruncateFlag',
	#superclass : 'FileOpeningFlag',
	#category : 'FileSystem-Base-FileOptions'
}

{ #category : 'accessing' }
FileTruncateFlag class >> defaultPlatformValue [
	^16r00200
]

{ #category : 'accessing' }
FileTruncateFlag class >> posixName [
	^'O_TRUNC'
]

{ #category : 'accessing' }
FileTruncateFlag class >> windowsValue [
	^16r80000
]

{ #category : 'convenience' }
FileTruncateFlag >> truncateRule [
	^self store class truncateExistingRule
]
