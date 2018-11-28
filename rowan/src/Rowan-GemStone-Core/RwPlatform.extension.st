Extension { #name : 'RwPlatform' }

{ #category : '*rowan-gemstone-core' }
RwPlatform class >> _userPlatformDictionary [

	"Platform globals are put into the current user's UserGlobals so that the values can be persisted.
		if the user does not have write access to UserGlobals, we'll store in session-specific dictionary"

	| userGlobals |
	userGlobals := System myUserProfile objectNamed: 'UserGlobals'.
	^ (System canWrite: userGlobals)
		ifTrue: [ userGlobals ]
		ifFalse: [ SessionTemps current ]

]

{ #category : '*rowan-gemstone-core' }
RwPlatform class >> _userPlatformDictionaryForUser: aUserId [

	"Platform globals are put into the current user's UserGlobals so that the values can be persisted.
		if the user does not have write access to UserGlobals, we'll store in session-specific dictionary"

	| userGlobals |
	userGlobals := (AllUsers userWithId: aUserId) objectNamed: 'UserGlobals'.
	^ (System canWrite: userGlobals)
		ifTrue: [ userGlobals ]
		ifFalse: [ SessionTemps current ]
]

{ #category : '*rowan-gemstone-core' }
RwPlatform class >> current [

	^ self _userPlatformDictionary 
		at: #RwUserPlatformInstance 
		ifAbsentPut: [ RwGsPlatform new ]

]

{ #category : '*rowan-gemstone-core' }
RwPlatform class >> reset [

	^ self _userPlatformDictionary 
		removeKey: #RwUserPlatformInstance 
		ifAbsent: [ ]

]
