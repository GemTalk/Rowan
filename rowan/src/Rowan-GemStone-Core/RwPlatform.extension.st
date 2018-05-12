Extension { #name : 'RwPlatform' }

{ #category : '*rowan-gemstone-core' }
RwPlatform class >> current [

	^ (System myUserProfile objectNamed: 'UserGlobals') 
		at: #RwUserPlatformInstance 
		ifAbsentPut: [ RwGsPlatform new ]
]
