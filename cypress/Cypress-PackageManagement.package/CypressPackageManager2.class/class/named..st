"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: Accessing
"
named: aKey
	"Answer the Package Manager previously saved under aKey.
	 It is an error if there was not one saved under that key."

	^self
		named: aKey
		or: [self error: 'No previously saved Package Manager under the key ', aKey printString]