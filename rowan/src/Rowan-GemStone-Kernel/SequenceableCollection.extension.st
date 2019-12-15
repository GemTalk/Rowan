Extension { #name : 'SequenceableCollection' }

{ #category : '*rowan-gemstone-kernel' }
SequenceableCollection >> copyUpTo: anObject [

	"Answer all elements up to but not including anObject. If there
  is no such object, answer a copy of the receiver."

	| idx |
	idx := self indexOf: anObject startingAt: 1.
	idx == 0
		ifTrue: [ ^ self copy ]
		ifFalse: [ ^ self copyFrom: 1 to: idx - 1 ]

]

{ #category : '*rowan-gemstone-kernel' }
SequenceableCollection class >> new: size withAll: value [

	"Answer an instance of me, with number of elements equal to size, each 
	of which refers to the argument, value."

	^ (self new: size)
		atAllPut: value;
		yourself

]
