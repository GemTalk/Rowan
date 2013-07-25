accessing
resolveGlobalNamed: aString

	^self resolveGlobalNamed: aString
		or: [CypressError signal: 'Could not resolve global named ' , aString printString]