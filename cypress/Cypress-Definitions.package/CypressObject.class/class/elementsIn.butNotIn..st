"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: miscellany
"
elementsIn: sourceCollection butNotIn: exclusionCollection

	| exclusionSet |
	exclusionSet := exclusionCollection asSet.
	^sourceCollection reject: [:each | exclusionSet includes: each]