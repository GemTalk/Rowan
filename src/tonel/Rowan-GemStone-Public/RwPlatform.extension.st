Extension { #name : 'RwPlatform' }

{ #category : '*rowan-gemstone-public' }
RwPlatform class >> current [

	^ (System myUserProfile objectNamed: 'UserGlobals') 
		at: #RwUserPlatformInstance 
		ifAbsentPut: [ RwGsPlatform new ]
]
