Extension { #name : 'FileStreamPortable' }

{ #category : '*FileSystem-Core' }
FileStreamPortable class >> onHandle: aFileSystemHandle [
	^ self concreteStream new
		open: aFileSystemHandle fullName
		forWrite: aFileSystemHandle isWritable
]
