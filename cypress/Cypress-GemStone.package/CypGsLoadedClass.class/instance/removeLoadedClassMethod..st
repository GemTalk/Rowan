"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: methods
"
removeLoadedClassMethod: aLoadedMethod

	self markPackageDirty.
	loadedClassMethods removeKey: aLoadedMethod key
		ifAbsent: [self error: 'Method not present in loadedClass']