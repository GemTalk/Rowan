Extension { #name : 'Rowan' }

{ #category : '*rowan-core' }
Rowan class >> initialize [

	"Rowan initialize."

	configuration := self platform defaultConfiguration.
	self platform initialize
]
