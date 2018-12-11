Extension { #name : 'FileSystem' }

{ #category : '*FileSystem-Disk' }
FileSystem >> isDiskFileSystem [
	^ store isDiskFileSystem
]

{ #category : '*filesystem-disk' }
FileSystem class >> * aFileOrDirectoryName [
	^ self disk * aFileOrDirectoryName
]

{ #category : '*filesystem-disk' }
FileSystem class >> / aFileOrDirectoryName [
	^ self disk / aFileOrDirectoryName
]

{ #category : '*filesystem-disk' }
FileSystem class >> client [
	"Answer a filesystem that represents the 'on-disk' filesystem used by the host operating system."

	^ DiskStore currentClientFileSystem
]

{ #category : '*filesystem-disk' }
FileSystem class >> clientWorkingDirectory [
	^ self disk clientWorkingDirectory
]

{ #category : '*filesystem-disk' }
FileSystem class >> disk [
	"Answer a filesystem that represents the 'on-disk' filesystem used by the host operating system."

	^ DiskStore currentFileSystem
]

{ #category : '*filesystem-disk' }
FileSystem class >> root [
	^ self disk root
]

{ #category : '*filesystem-disk' }
FileSystem class >> workingDirectory [
	^ self disk workingDirectory
]
