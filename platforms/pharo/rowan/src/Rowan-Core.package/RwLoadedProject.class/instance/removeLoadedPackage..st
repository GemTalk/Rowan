accessing
removeLoadedPackage: aLoadedPackage

	self markDirty.
	loadedPackages removeKey: aLoadedPackage key