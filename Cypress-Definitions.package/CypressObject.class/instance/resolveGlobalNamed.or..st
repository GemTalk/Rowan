accessing
resolveGlobalNamed: aString or: aBlock

	^((System myUserProfile resolveSymbol: aString) ifNil: [^aBlock value])
		value