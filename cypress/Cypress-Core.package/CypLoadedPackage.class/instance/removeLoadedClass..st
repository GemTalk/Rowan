"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: accessing
"
removeLoadedClass: aLoadedClass

	self markDirty.
	loadedClasses remove: aLoadedClass
		ifAbsent: [self error: 'Class not present in package.'].
	aLoadedClass setPackage: nil