automatic class initialization
automaticClassInitializationBlackList_global

	"Answer global list of project names for which automatic class initialiation should be disabled."

	| preferenceSymbol |
	preferenceSymbol := self _automaticClassInitializationBlackList_symbol.
	^ self 
		globalPreferenceFor: preferenceSymbol 
		ifAbsent: [
			| list |
			list := OrderedCollection new.
			self setDefaultPreferenceFor: preferenceSymbol to: list.
			list]