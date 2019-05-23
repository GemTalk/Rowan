comparing
= aRwGemStoneVersionNumber

	| condensed aCondensed |
	aRwGemStoneVersionNumber species = self species
		ifFalse: [ ^ false ].
	condensed := self collapseZeros.
	aCondensed := aRwGemStoneVersionNumber collapseZeros.
	(condensed ~~ self or: [ aCondensed ~~ aRwGemStoneVersionNumber ])
		ifTrue: [ ^ condensed compareEqualTo: aCondensed ].
	^ self compareEqualTo: aRwGemStoneVersionNumber
