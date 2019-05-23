accessing
addLoadedClassExtension: aLoadedClassExtension

	self markDirty.
	loadedClassExtensions at: aLoadedClassExtension key
		put: aLoadedClassExtension.
	aLoadedClassExtension loadedPackage: self