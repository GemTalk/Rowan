"
notice: Copyright 2013 GemTalk Systems, LLC. Open source under the MIT license.
category: *Cypress-GemStoneCompatibilityLayer
"
difference: aCollection
	"Answer the set theoretic difference of two collections."

	| set |
	set := self asSet.
	aCollection do: [:each | set remove: each ifAbsent: []].
	^self species withAll: set asArray