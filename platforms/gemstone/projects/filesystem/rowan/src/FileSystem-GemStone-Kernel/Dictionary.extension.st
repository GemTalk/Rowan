Extension { #name : 'Dictionary' }

{ #category : '*filesystem-gemstone-kernel' }
Dictionary >> associations [
	"Answer a Collection containing the receiver's associations."

	| result |
	result := WriteStream on: (Array new: self size).
	self associationsDo: [ :assoc | result nextPut: assoc ].
	^ result contents

]
