private
addDefinition: aDefinition to: aDictionary

	| key |
	key := aDefinition key.
	(aDictionary includesKey: key)
		ifTrue: [self error: 'Duplicate definition.'].
	^ aDictionary at: key put: aDefinition