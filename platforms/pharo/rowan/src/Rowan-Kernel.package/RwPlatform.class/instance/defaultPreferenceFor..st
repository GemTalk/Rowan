preferences
defaultPreferenceFor: preferenceSymbol

	^ self defaultPreferenceFor: preferenceSymbol ifAbsent: [ self error: 'No preference found for ', preferenceSymbol asString printString ]