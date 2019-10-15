Extension { #name : 'FileStreamPortable' }

{ #category : '*filesystem-gemstone-kernel' }
FileStreamPortable class >> onHandle: aFileSystemHandle [
	^ self concreteStream new
		open: aFileSystemHandle fullName
		forWrite: aFileSystemHandle isWritable
]
