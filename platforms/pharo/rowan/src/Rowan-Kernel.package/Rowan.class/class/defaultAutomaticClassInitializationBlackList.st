public
defaultAutomaticClassInitializationBlackList

	"Answer default list of project names for which automatic class initialiation should be disabled.
		Individual users may override the black list."

	^ self platform automaticClassInitializationBlackList_default