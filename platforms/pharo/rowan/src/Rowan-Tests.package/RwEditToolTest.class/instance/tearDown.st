running
tearDown

	| preferenceSymbol |

	super tearDown.

"clean up blackList"
	preferenceSymbol := Rowan platform _automaticClassInitializationBlackList_symbol.
	globalBlackList
		ifNil: [ Rowan clearDefaultAutomaticClassInitializationBlackList ]
		ifNotNil: [
			Rowan platform 
				setDefaultPreferenceFor: preferenceSymbol 
				to: globalBlackList ].
	userBlackList
		ifNil: [ Rowan clearUserAutomaticClassInitializationBlackList ]
		ifNotNil: [
			Rowan platform 
				setUserPreferenceFor: preferenceSymbol 
				to: userBlackList ].
	sessionBlackList
		ifNil: [ Rowan clearSessionAutomaticClassInitializationBlackList ]
		ifNotNil: [
			Rowan platform 
				setSessionPreferenceFor: preferenceSymbol 
				to: sessionBlackList ].