accessing
addLoadedPackage: aLoadedPackage

	self markDirty.
	loadedPackages at: aLoadedPackage key put: aLoadedPackage.
	aLoadedPackage loadedProject: self