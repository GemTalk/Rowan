"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: miscellany
"
elementsInBoth: collection1 and: collection2
	"Set intersection generalized to any collections."

	| temporarySet |
	temporarySet := collection2 asSet.
	^collection1 select: [:each | temporarySet includes: each]