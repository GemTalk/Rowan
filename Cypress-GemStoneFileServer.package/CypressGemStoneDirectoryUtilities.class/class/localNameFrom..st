"
notice: This work is protected by copyright. All rights reserved.
category: utilities
"
localNameFrom: aDirectory

	| endOfPath |
	endOfPath := aDirectory findLastSubString: self pathNameDelimiter startingAt: aDirectory size.
	^aDirectory copyFrom: endOfPath + 1 to: aDirectory size