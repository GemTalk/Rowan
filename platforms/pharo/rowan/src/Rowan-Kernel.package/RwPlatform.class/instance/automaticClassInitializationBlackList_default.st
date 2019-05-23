automatic class initialization
automaticClassInitializationBlackList_default

	"Answer default list of project names for which automatic class initialiation should be disabled."

	| preferenceSymbol |
	preferenceSymbol := self _automaticClassInitializationBlackList_symbol.
	^ self 
		defaultPreferenceFor: preferenceSymbol 
		ifAbsent: [
			| list |
			list := OrderedCollection new.
			self setDefaultPreferenceFor: preferenceSymbol to: list.
			list]