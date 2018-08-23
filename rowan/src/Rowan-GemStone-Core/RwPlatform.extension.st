Extension { #name : 'RwPlatform' }

{ #category : '*rowan-gemstone-core' }
RwPlatform class >> current [

	| userGlobals |
	userGlobals := System myUserProfile objectNamed: 'UserGlobals'.
	^ (System canWrite: userGlobals)
		ifTrue: [
			userGlobals 
				at: #RwUserPlatformInstance 
				ifAbsentPut: [ RwGsPlatform new ] ]
		ifFalse: [
			SessionTemps current 
				at: #RwUserPlatformInstance 
				ifAbsentPut: [ RwGsPlatform new ] ]

]

{ #category : '*rowan-gemstone-core' }
RwPlatform class >> reset [

	^ (System myUserProfile objectNamed: 'UserGlobals') 
		removeKey: #RwUserPlatformInstance 
		ifAbsent: [ ]
]
