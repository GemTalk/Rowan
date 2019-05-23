accessing
removeClassMethod: selector

	| definition |
	definition := classMethodDefinitions
		at: selector
		ifAbsent: [ self error: 'No method definition for ' , selector printString , ' found.' ].
	self removeDefinition: definition from: classMethodDefinitions.
	^ definition
