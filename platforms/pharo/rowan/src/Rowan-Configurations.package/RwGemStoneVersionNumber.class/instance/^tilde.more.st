comparing
~> aRwGemStoneVersionNumber

	aRwGemStoneVersionNumber size == 1 ifTrue: [ ^false ].
	^self >= aRwGemStoneVersionNumber and: [ self < aRwGemStoneVersionNumber approximateBase ]
