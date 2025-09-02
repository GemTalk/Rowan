Extension { #name : 'AbstractFraction' }

{ #category : '*ston-gemstone-kernel' }
AbstractFraction >> stonOn: stonWriter [

	stonWriter writeFraction: self
]
