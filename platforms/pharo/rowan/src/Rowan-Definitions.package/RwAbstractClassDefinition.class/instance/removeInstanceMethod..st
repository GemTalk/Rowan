accessing
removeInstanceMethod: selector

	| definition |
	definition := instanceMethodDefinitions
		at: selector
		ifAbsent: [ self error: 'No method definition for ' , selector printString , ' found.' ].
	self removeDefinition: definition from: instanceMethodDefinitions.
	^ definition
