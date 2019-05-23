test preferences (issue #448)
testPreferences

	"https://github.com/GemTalk/Rowan/issues/448"

	| platformInstance  x preference |
	platformInstance := Rowan platform.
	preference := self _testPreference.

	self assert: (platformInstance isKindOf: RwPlatform).

	platformInstance clearPreferenceFor: preference.
	self should: [ x := platformInstance preferenceFor: preference ] raise: Error.

	platformInstance setPreferenceFor: preference to: true.
	self assert: (platformInstance preferenceFor: preference).