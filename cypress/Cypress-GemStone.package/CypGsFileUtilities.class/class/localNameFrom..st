"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: utilities
"
localNameFrom: aDirectory

	| endOfPath |
	endOfPath := aDirectory findLastSubString: self pathNameDelimiter
				startingAt: aDirectory size.
	^aDirectory copyFrom: endOfPath + 1 to: aDirectory size