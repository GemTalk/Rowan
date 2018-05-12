Extension { #name : 'Rowan' }

{ #category : '*rowan-initialize' }
Rowan class >> initialize [

	"Rowan initialize."

	configuration := self platform defaultConfiguration.
	self platform initialize
]

{ #category : '*rowan-initialize' }
Rowan class >> platform [

	^ RwPlatform current
]
