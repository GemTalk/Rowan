test preferences (issue #448)
testUserPreferences

	"https://github.com/GemTalk/Rowan/issues/448"

	| platformInstance  x preference |
	platformInstance := Rowan platform.
	preference := self _testPreference.

	platformInstance clearUserPreferenceFor: preference.
	self should: [ x := platformInstance userPreferenceFor: preference ] raise: Error.

	platformInstance setUserPreferenceFor: preference to: true.
	self assert: (platformInstance preferenceFor: preference).