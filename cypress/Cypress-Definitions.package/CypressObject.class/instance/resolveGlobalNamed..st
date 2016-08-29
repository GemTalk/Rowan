"
notice: Copyright 2013 GemTalk Systems, LLC. Open source under the MIT license.
category: accessing
"
resolveGlobalNamed: aString

	^self resolveGlobalNamed: aString
		or: [CypressError signal: 'Could not resolve global named ' , aString printString]