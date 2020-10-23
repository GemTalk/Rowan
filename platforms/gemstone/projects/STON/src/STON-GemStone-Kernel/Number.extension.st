Extension { #name : 'Number' }

{ #category : '*ston-gemstone-kernel' }
Number >> stonContainSubObjects [ 
	^ false

]

{ #category : '*ston-gemstone-kernel' }
Number >> stonOn: stonWriter [
	stonWriter writeFloat: self asFloat

]
