automatic class initialization
automaticClassInitializationBlackList

	"Answer list of project names for which automatic class initialiation should be disabled."

	| preferenceSymbol |
	preferenceSymbol := self _automaticClassInitializationBlackList_symbol.
	^ self 
		preferenceFor: preferenceSymbol 
		ifAbsent: [
			| list |
			list := OrderedCollection new.
			self setPreferenceFor: preferenceSymbol to: list.
			list]