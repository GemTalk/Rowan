Extension { #name : 'PositionableStream' }

{ #category : '*filesystem-gemstone-kernel' }
PositionableStream >> back [
	"Go back one element and return it."

	self position = 0 ifTrue: [self positionError].
	self skip: -1.
	^ self peek

]

{ #category : '*filesystem-gemstone-kernel' }
PositionableStream >> collectionSpecies [

	^self _collection species

]

{ #category : '*filesystem-gemstone-kernel' }
PositionableStream >> positionError [
	"Since I am not necessarily writable, it is up to my subclasses to override 
	position: if expanding the collection is preferrable to giving this error."

	self error: 'Attempt to set the position of a PositionableStream out of bounds'

]

{ #category : '*filesystem-gemstone-kernel' }
PositionableStream >> setFrom: newStart to: newStop [

	position := newStart - 1.
	readLimit := newStop
]
