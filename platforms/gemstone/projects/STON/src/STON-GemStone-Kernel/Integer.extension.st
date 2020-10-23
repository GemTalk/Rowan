Extension { #name : 'Integer' }

{ #category : '*ston-gemstone-kernel' }
Integer >> stonOn: stonWriter [
	stonWriter writeInteger: self

]
