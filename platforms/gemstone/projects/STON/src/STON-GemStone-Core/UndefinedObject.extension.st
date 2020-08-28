Extension { #name : 'UndefinedObject' }

{ #category : '*ston-gemstone-core' }
UndefinedObject >> stonContainSubObjects [ 
	^ false

]

{ #category : '*ston-gemstone-core' }
UndefinedObject >> stonOn: stonWriter [
	stonWriter writeNull

]
