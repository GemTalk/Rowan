testing
isDirty

	"a project is dirty if it has changes that are not written to disk, or it's packages have changes that are not written to  disk."

	isDirty ifTrue: [ ^ true].
	self loadedPackages
		do: [ :loadedPackage | 
			loadedPackage isDirty
				ifTrue: [ ^ true ] ].
	^ false
