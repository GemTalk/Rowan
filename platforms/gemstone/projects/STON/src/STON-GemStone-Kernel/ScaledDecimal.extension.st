Extension { #name : 'ScaledDecimal' }

{ #category : '*ston-gemstone-kernel' }
ScaledDecimal >> stonOn: stonWriter [

	stonWriter writeScaledDecimal: self
]
