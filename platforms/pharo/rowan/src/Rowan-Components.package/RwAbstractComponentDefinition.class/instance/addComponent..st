accessing
addComponent: aComponentDefinition

	| key |
	key := aComponentDefinition key.
	(components includesKey: key) ifTrue: [self error: 'Duplicate component'].
	^ components at: key put: aComponentDefinition