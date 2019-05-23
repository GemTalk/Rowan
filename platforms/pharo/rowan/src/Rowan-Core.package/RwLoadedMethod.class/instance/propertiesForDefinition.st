definitions
propertiesForDefinition

	| props |
	props := super propertiesForDefinition.
	props at: 'selector' put: name.
	^props