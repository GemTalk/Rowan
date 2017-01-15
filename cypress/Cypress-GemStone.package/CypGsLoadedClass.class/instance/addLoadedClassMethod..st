"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: methods
"
addLoadedClassMethod: aLoadedMethod

	self markPackageDirty.
	loadedClassMethods at: aLoadedMethod key put: aLoadedMethod.
	aLoadedMethod loadedClass: self