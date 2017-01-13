"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: unknown
"
addLoadedExtendedClass: aLoadedExtendedClass

	self markDirty.
	loadedExtendedClasses at: aLoadedExtendedClass key
		put: aLoadedExtendedClass.
	aLoadedExtendedClass loadedPackage: self