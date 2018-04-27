Extension { #name : 'RwPlatform' }

{ #category : '*rowan-gemstone-core' }
RwPlatform class >> forGemStone [

	| platformClass |
	platformClass := RwPlatformSpecificNameResolver
				resolve: 'RwGsPlatform'
				ifAbsent: 
					[self
						error: 'The CypressGemStone prerequisite package does not seem to be installed.'].
	^platformClass new
]
