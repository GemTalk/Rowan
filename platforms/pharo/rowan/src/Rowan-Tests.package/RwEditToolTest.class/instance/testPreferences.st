test api
testPreferences

	| projectName |
	projectName := '__Test-Preferences__'.

"standard API - user in GemStone"
	Rowan automaticClassInitializationBlackList add: projectName.
	self assert: (Rowan automaticClassInitializationBlackList includes: projectName).

	Rowan clearAutomaticClassInitializationBlackList.
	self deny: (Rowan automaticClassInitializationBlackList includes: projectName).

"globals"
	Rowan globalAutomaticClassInitializationBlackList add: projectName.
	self assert: (Rowan globalAutomaticClassInitializationBlackList includes: projectName).

	Rowan clearGlobalAutomaticClassInitializationBlackList.
	self deny: (Rowan globalAutomaticClassInitializationBlackList includes: projectName).

"users"
	Rowan userAutomaticClassInitializationBlackList add: projectName.
	self assert: (Rowan userAutomaticClassInitializationBlackList includes: projectName).

	Rowan clearUserAutomaticClassInitializationBlackList.
	self deny: (Rowan userAutomaticClassInitializationBlackList includes: projectName).

"session"
	Rowan  sessionAutomaticClassInitializationBlackList add: projectName.
	self assert: (Rowan sessionAutomaticClassInitializationBlackList includes: projectName).

	Rowan clearSessionAutomaticClassInitializationBlackList.
	self deny: (Rowan sessionAutomaticClassInitializationBlackList includes: projectName).