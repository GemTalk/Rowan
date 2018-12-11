Class {
	#name : 'RwPlatformInstanceTest',
	#superclass : 'RwToolTest',
	#category : 'Rowan-Tests'
}

{ #category : 'private' }
RwPlatformInstanceTest >> _testPreference [

	^#'unknown_preference'
]

{ #category : 'running' }
RwPlatformInstanceTest >> tearDown [

	super tearDown.
	Rowan platform clearAllPreferencesFor: self _testPreference
]

{ #category : 'tests' }
RwPlatformInstanceTest >> testGlobalPreferences [

	"https://github.com/GemTalk/Rowan/issues/448"

	| platformInstance  x preference |
	platformInstance := Rowan platform.
	preference := self _testPreference.

	platformInstance clearGlobalPreferenceFor: preference.
	self should: [ x := platformInstance globalPreferenceFor: preference ] raise: Error.

	platformInstance setGlobalPreferenceFor: preference to: true.
	self assert: (platformInstance preferenceFor: preference).
]

{ #category : 'tests' }
RwPlatformInstanceTest >> testPreferencePrecedence [

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
	self assert: (x := platformInstance preferenceFor: preference) == #default. 	"session prefs cleared and user prefs set"

	platformInstance setSessionPreferenceFor: preference to: #session.
	platformInstance clearPreferenceFor: preference.										"session and user prefs cleared"
	self assert: (x := platformInstance preferenceFor: preference) == #global.
]

{ #category : 'tests' }
RwPlatformInstanceTest >> testPreferences [

	"https://github.com/GemTalk/Rowan/issues/448"

	| platformInstance  x preference |
	platformInstance := Rowan platform.
	preference := self _testPreference.

	self assert: (platformInstance isKindOf: RwPlatform).

	platformInstance clearPreferenceFor: preference.
	self should: [ x := platformInstance preferenceFor: preference ] raise: Error.

	platformInstance setPreferenceFor: preference to: true.
	self assert: (platformInstance preferenceFor: preference).
]

{ #category : 'tests' }
RwPlatformInstanceTest >> testSessionPreferences [

	"https://github.com/GemTalk/Rowan/issues/448"

	| platformInstance  x preference |
	platformInstance := Rowan platform.
	preference := self _testPreference.

	platformInstance clearSessionPreferenceFor: preference.
	self should: [ x := platformInstance sessionPreferenceFor: preference ] raise: Error.

	platformInstance setSessionPreferenceFor: preference to: true.
	self assert: (platformInstance preferenceFor: preference).
]

{ #category : 'tests' }
RwPlatformInstanceTest >> testUserPreferences [

	"https://github.com/GemTalk/Rowan/issues/448"

	| platformInstance  x preference |
	platformInstance := Rowan platform.
	preference := self _testPreference.

	platformInstance clearUserPreferenceFor: preference.
	self should: [ x := platformInstance userPreferenceFor: preference ] raise: Error.

	platformInstance setUserPreferenceFor: preference to: true.
	self assert: (platformInstance preferenceFor: preference).
]
