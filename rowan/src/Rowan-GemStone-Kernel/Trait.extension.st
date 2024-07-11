Extension { #name : 'Trait' }

{ #category : '*rowan-gemstone-kernel' }
Trait >> rowanProjectName [
	"answer the name of the project that the receiver is a member of. Answer `Rowan unpackagedName` if 
		the receiver is not a member of any project"

	| loadedTrait |
	loadedTrait := Rowan image
		loadedTraitForTrait: self
		ifAbsent: [ ^ Rowan unpackagedName ].
	^ loadedTrait loadedProject name
]
