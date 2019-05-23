running
setUp

	| preferenceSymbol |
	super setUp.

"get current black list values"
	preferenceSymbol := Rowan platform _automaticClassInitializationBlackList_symbol.
	globalBlackList :=	(Rowan platform 
		globalPreferenceFor: preferenceSymbol 
		ifAbsent: []) copy.
	userBlackList :=	(Rowan platform 
		userPreferenceFor: preferenceSymbol 
		ifAbsent: []) copy.
	sessionBlackList :=	(Rowan platform 
		sessionPreferenceFor: preferenceSymbol 
		ifAbsent: []) copy.