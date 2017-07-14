"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: encoding
"
isCharacterSafeForHttp: aChar
	"Answer whether a character is 'safe', or needs to be escaped when used, eg, in a URL."

	^aChar codePoint < 128
		and: [aChar isAlphaNumeric or: ['.-_' includes: aChar]]
