"
notice: This work is protected by copyright. All rights reserved.
category: accessing
"
resolveGlobalNamed: aString

	^self resolveGlobalNamed: aString
		or: [CypressError signal: 'Could not resolve global named ' , aString printString]