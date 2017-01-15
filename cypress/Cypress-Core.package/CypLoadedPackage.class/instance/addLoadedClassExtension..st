"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: accessing
"
addLoadedClassExtension: aLoadedClassExtension

	self markDirty.
	loadedClassExtensions at: aLoadedClassExtension key
		put: aLoadedClassExtension.
	aLoadedClassExtension loadedPackage: self