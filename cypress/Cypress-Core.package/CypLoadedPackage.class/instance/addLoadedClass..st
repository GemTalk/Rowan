"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: accessing
"
addLoadedClass: aLoadedClass

	self markDirty.
	loadedClasses add: aLoadedClass.
	aLoadedClass loadedPackage: self