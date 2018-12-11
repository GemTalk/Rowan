Extension { #name : 'Rowan' }

{ #category : '*rowan-gemstone-core' }
Rowan class >> sessionAutomaticClassInitializationBlackList [

	"Answer session list of project names for which automatic class initialiation should be disabled.
		Session black list only active for duration of GemStone session."

	^ self platform automaticClassInitializationBlackList_session
]

{ #category : '*rowan-gemstone-core' }
Rowan class >> userAutomaticClassInitializationBlackList [

	"Answer user list of project names for which automatic class initialiation should be disabled.
		user black list only applies to current user."

	^ self platform automaticClassInitializationBlackList_user
]
