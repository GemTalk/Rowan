"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: *Cypress-GemStoneCompatibilityLayer
"
findString: subString startingAt: startIndex caseSensitive: aBoolean
	"If a receiver contains subString beginning at some point at or after
	 startIndex, this returns the index at which subString begins.  If the
	 receiver does not contain subString, this returns 0."

	^self
		_findString: subString
		startingAt: startIndex
		ignoreCase: aBoolean not