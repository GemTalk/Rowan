"
notice: This work is protected by copyright. All rights reserved.
category: accessing
"
resolveGlobalNamed: aString or: aBlock

	^((System myUserProfile resolveSymbol: aString) ifNil: [^aBlock value])
		value