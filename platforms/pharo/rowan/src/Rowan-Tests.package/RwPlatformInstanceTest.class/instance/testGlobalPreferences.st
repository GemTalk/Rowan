test preferences (issue #448)
testGlobalPreferences

	"https://github.com/GemTalk/Rowan/issues/448"

	| platformInstance  x preference |
	platformInstance := Rowan platform.
	preference := self _testPreference.

	platformInstance clearGlobalPreferenceFor: preference.
	self should: [ x := platformInstance globalPreferenceFor: preference ] raise: Error.

	platformInstance setGlobalPreferenceFor: preference to: true.
	self assert: (platformInstance preferenceFor: preference).