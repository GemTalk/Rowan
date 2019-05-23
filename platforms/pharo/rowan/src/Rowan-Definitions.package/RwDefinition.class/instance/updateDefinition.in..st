private
updateDefinition: aDefinition in: aDictionary

	| key |
	key := aDefinition key.
	aDictionary at: key put: aDefinition