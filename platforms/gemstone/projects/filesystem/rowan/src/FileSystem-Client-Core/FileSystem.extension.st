Extension { #name : 'FileSystem' }

{ #category : '*FileSystem-Client-Core' }
FileSystem class >> clientDisk [
	"Answer a filesystem that represents the 'on-disk' filesystem used by the host operating system."

	^ ClientStore currentFileSystem
]
