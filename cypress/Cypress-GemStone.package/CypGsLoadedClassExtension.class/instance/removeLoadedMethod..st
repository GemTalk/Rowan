"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: methods
"
removeLoadedMethod: aLoadedMethod

	aLoadedMethod classIsMeta
		ifTrue: [self removeLoadedClassMethod: aLoadedMethod]
		ifFalse: [self removeLoadedInstanceMethod: aLoadedMethod]