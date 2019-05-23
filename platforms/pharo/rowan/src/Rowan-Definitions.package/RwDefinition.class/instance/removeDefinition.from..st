private
removeDefinition: aDefinition from: aDictionary

	| key |
	key := aDefinition key.
	aDictionary
		removeKey: key
		ifAbsent: [ self error: 'no definition for ' , key printString , ' found' ]