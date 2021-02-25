Extension { #name : 'Boolean' }

{ #category : '*ston-gemstone-kernel' }
Boolean >> stonContainSubObjects [ 
	^ false

]

{ #category : '*ston-gemstone-kernel' }
Boolean >> stonOn: stonWriter [
	stonWriter writeBoolean: self

]
