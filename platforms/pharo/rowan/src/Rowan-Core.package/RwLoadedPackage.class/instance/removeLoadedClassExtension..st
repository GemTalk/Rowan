accessing
removeLoadedClassExtension: aLoadedClassExtension

	self markDirty.
	loadedClassExtensions removeKey: aLoadedClassExtension key.
	aLoadedClassExtension loadedPackage: nil