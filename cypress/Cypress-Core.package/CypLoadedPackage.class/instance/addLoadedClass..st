"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: accessing
"
addLoadedClass: aLoadedClass

	self markDirty.
	loadedClasses at: aLoadedClass key put: aLoadedClass.
	aLoadedClass loadedPackage: self