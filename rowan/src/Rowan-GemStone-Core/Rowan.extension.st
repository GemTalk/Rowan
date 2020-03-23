Extension { #name : 'Rowan' }

{ #category : '*rowan-gemstone-core' }
Rowan class >> clearGlobalAutomaticClassInitializationBlackList [

	"Clear global list of project names for which automatic class initialiation should be disabled.
		user black list only applies to current user."

	^ self platform clearAutomaticClassInitializationBlackList_global
]

{ #category : '*rowan-gemstone-core' }
Rowan class >> clearSessionAutomaticClassInitializationBlackList [

	"Clear session list of project names for which automatic class initialiation should be disabled.
		Session black list only active for duration of GemStone session."

	^ self platform clearAutomaticClassInitializationBlackList_session
]

{ #category : '*rowan-gemstone-core' }
Rowan class >> clearUserAutomaticClassInitializationBlackList [

	"Clear user list of project names for which automatic class initialiation should be disabled.
		user black list only applies to current user."

	^ self platform clearAutomaticClassInitializationBlackList_user
]

{ #category : '*rowan-gemstone-core' }
Rowan class >> gemstoneTools [

	^self platform gemstoneTools
]

{ #category : '*rowan-gemstone-core' }
Rowan class >> globalAutomaticClassInitializationBlackList [

	"Answer global list of project names for which automatic class initialiation should be disabled.
		Session black list only active for duration of GemStone session."

	^ self platform automaticClassInitializationBlackList_global
]

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
