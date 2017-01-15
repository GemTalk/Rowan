"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: methods
"
addLoadedMethod: aLoadedMethod

	aLoadedMethod classIsMeta
		ifTrue: [self addLoadedClassMethod: aLoadedMethod]
		ifFalse: [self addLoadedInstanceMethod: aLoadedMethod]