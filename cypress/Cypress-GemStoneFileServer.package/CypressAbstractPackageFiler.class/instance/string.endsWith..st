"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: private
"
string: aString endsWith: subString
	"Answer whether the last characters of aString are the same as subString."

	| expectedStart |
	expectedStart := aString size - subString size + 1 max: 1.
	^expectedStart
		= (aString indexOfSubCollection: subString startingAt: expectedStart)
