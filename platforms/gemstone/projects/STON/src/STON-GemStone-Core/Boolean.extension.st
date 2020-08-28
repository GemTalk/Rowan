Extension { #name : 'Boolean' }

{ #category : '*ston-gemstone-core' }
Boolean >> stonContainSubObjects [ 
	^ false

]

{ #category : '*ston-gemstone-core' }
Boolean >> stonOn: stonWriter [
	stonWriter writeBoolean: self

]
