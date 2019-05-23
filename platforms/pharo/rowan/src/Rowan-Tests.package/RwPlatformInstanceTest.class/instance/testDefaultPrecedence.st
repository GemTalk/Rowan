test preferences (issue #448)
testDefaultPrecedence

	"https://github.com/GemTalk/Rowan/issues/448"

	| platformInstance  x preference |
	platformInstance := Rowan platform.
	preference := self _testPreference.

	platformInstance setDefaultPreferenceFor: preference to: #default.
	self assert: (x := platformInstance preferenceFor: preference) == #default.

	platformInstance setPreferenceFor: preference to: #user.
	self assert: (x := platformInstance preferenceFor: preference) == #user. 

	self assert: (x := platformInstance defaultPreferenceFor: preference) == #default. 

"testing GemStone implementation"
	self assert: (x := platformInstance  globalPreferenceFor: preference ifAbsent: [#absent ]) == #default. 
	self assert: (x := platformInstance  userPreferenceFor: preference ifAbsent: [#absent ]) == #user. 
	self assert: (x := platformInstance  sessionPreferenceFor: preference ifAbsent: [ #absent ]) == #absent. 

	platformInstance clearPreferenceFor: preference.
	platformInstance clearDefaultPreferenceFor: preference.
	self should: [ platformInstance preferenceFor: preference ] raise: Error