test preferences (issue #448)
testSessionPreferences

	"https://github.com/GemTalk/Rowan/issues/448"

	| platformInstance  x preference |
	platformInstance := Rowan platform.
	preference := self _testPreference.

	platformInstance clearSessionPreferenceFor: preference.
	self should: [ x := platformInstance sessionPreferenceFor: preference ] raise: Error.

	platformInstance setSessionPreferenceFor: preference to: true.
	self assert: (platformInstance preferenceFor: preference).