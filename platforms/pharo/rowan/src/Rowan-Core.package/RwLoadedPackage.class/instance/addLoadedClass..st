accessing
addLoadedClass: aLoadedClass

	self markDirty.
	loadedClasses at: aLoadedClass key put: aLoadedClass.
	aLoadedClass loadedPackage: self