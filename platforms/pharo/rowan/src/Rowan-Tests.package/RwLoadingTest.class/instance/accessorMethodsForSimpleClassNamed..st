support - simple definitions
accessorMethodsForSimpleClassNamed: name

	| result |
	result := Dictionary new.

	{('iv1In' , name).
	('iv2In' , name)}
		do: [ :ivName | 
			result
				at: ivName asSymbol
					put:
						(RwMethodDefinition
								withProperties:
									(Dictionary with: 'selector' -> ivName asSymbol with: 'protocol' -> 'accessing')
								source: ivName , ' ^' , ivName);
				at: (ivName , ':') asSymbol
					put:
						(RwMethodDefinition
								withProperties:
									(Dictionary
										with: 'selector' -> (ivName , ':') asSymbol
										with: 'protocol' -> 'accessing')
								source: ivName , ': anObject ' , ivName , ' := anObject') ].
	^ result