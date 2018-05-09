Extension { #name : 'Rowan' }

{ #category : '*rowan-initialize' }
Rowan class >> initialize [

	"Rowan initialize."

	configuration := self platform defaultConfiguration.
	self platform initialize
]
