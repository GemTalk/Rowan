automatic class initialization
clearAutomaticClassInitializationBlackList_global

	"Answer default list of project names for which automatic class initialiation should be disabled."

	| preferenceSymbol |
	preferenceSymbol := self _automaticClassInitializationBlackList_symbol.
	self clearGlobalPreferenceFor: preferenceSymbol