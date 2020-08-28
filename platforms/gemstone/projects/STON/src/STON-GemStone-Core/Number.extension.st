Extension { #name : 'Number' }

{ #category : '*ston-gemstone-core' }
Number >> stonContainSubObjects [ 
	^ false

]

{ #category : '*ston-gemstone-core' }
Number >> stonOn: stonWriter [
	stonWriter writeFloat: self asFloat

]
