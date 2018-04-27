Extension { #name : 'RwPlatform' }

{ #category : '*rowan-gemstone-core' }
RwPlatform class >> forGemStone [

	^ UserGlobals at: #RwUserPlatformInstance ifAbsentPut: [ RwGsPlatform new ]
]
