preferences
preferenceFor: preferenceSymbol

	^ self preferenceFor: preferenceSymbol ifAbsent: [ self error: 'No preference found for ', preferenceSymbol asString printString ]