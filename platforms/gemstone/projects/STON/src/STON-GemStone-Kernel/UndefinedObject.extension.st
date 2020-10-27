Extension { #name : 'UndefinedObject' }

{ #category : '*ston-gemstone-kernel' }
UndefinedObject >> stonContainSubObjects [ 
	^ false

]

{ #category : '*ston-gemstone-kernel' }
UndefinedObject >> stonOn: stonWriter [
	stonWriter writeNull

]
