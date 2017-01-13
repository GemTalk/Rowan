"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: accessing
"
removeLoadedMethod: aLoadedMethod

	self markPackageDirty.
	loadedMethods removeKey: aLoadedMethod key
		ifAbsent: [self error: 'Method not present in loadedClass']