accessing
removeLoadedClass: aLoadedClass

	self markDirty.
	loadedClasses removeKey: aLoadedClass key.
	aLoadedClass loadedPackage: nil