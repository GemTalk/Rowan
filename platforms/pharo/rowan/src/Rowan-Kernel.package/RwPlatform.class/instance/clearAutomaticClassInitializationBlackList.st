automatic class initialization
clearAutomaticClassInitializationBlackList

	"Answer list of project names for which automatic class initialiation should be disabled."

	| preferenceSymbol |
	preferenceSymbol := self _automaticClassInitializationBlackList_symbol.
	self clearPreferenceFor: preferenceSymbol