*Cypress-Structure
isSafeForHTTP
	"Answer whether a character is 'safe', or needs to be escaped when used, eg, in a URL."

	^self codePoint < 128
		and: [self isAlphaNumeric or: ['.-_' includes: self]]