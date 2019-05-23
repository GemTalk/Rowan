accessing
addDefinition: aDefinition

	| key |
	key := aDefinition key.
	(definitions includesKey: key)
		ifTrue: [ self error: 'Duplicate definition ' ].
	definitions at: key put: aDefinition