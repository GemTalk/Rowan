test preferences (issue #448)
testPreferencePrecedence

	"https://github.com/GemTalk/Rowan/issues/448"

	| platformInstance  x preference |
	platformInstance := Rowan platform.
	preference := self _testPreference.

	platformInstance setGlobalPreferenceFor: preference to: #global.
	self assert: (platformInstance preferenceFor: preference) == #global.

	platformInstance setUserPreferenceFor: preference to: #user.
	self assert: (platformInstance preferenceFor: preference) == #user.

	platformInstance setSessionPreferenceFor: preference to: #session.
	self assert: (platformInstance preferenceFor: preference) == #session.

	platformInstance setPreferenceFor: preference to: #default.
	self assert: (x := platformInstance preferenceFor: preference) == #default. 

	self assert: (x := platformInstance userPreferenceFor: preference ifAbsent: [ #absent ]) == #default. 		"session prefs cleared and user prefs set"
	self assert: (x := platformInstance sessionPreferenceFor: preference ifAbsent: [ #absent ]) == #absent. 	"session prefs cleared and user prefs set"

	platformInstance setSessionPreferenceFor: preference to: #session.
	platformInstance clearPreferenceFor: preference.										"session and user prefs cleared"
	self assert: (x := platformInstance preferenceFor: preference) == #global.