private
compareEqualTo: aRwGemStoneVersionNumber

	| mySize |
	aRwGemStoneVersionNumber species = self species ifFalse: [ ^false ].
	mySize := self size.
	mySize = aRwGemStoneVersionNumber size 
		ifFalse: [ ^false ].
	1 to: mySize do: [:i |
		(self at: i) = (aRwGemStoneVersionNumber at: i) ifFalse: [ ^false ]].
	^true
