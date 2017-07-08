"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: accessing
"
resolveGlobalNamed: aString or: aBlock

	^((System myUserProfile resolveSymbol: aString) ifNil: [^aBlock value])
		value
