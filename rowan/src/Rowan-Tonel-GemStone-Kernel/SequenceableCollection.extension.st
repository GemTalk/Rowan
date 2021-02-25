Extension { #name : 'SequenceableCollection' }

{ #category : '*rowan-tonel-gemstone-kernel' }
SequenceableCollection >> allButLast [

	"Answer a copy of the receiver containing all but the last
	element. Raise an error if there are not enough elements."

	^ self allButLast: 1

]

{ #category : '*rowan-tonel-gemstone-kernel' }
SequenceableCollection >> allButLast: n [

	"Answer a copy of the receiver containing all but the last n
	elements. Raise an error if there are not enough elements."

	^ self copyFrom: 1 to: self size - n

]

{ #category : '*rowan-tonel-gemstone-kernel' }
SequenceableCollection >> beginsWith: aSequenceableCollection [

	(aSequenceableCollection isEmpty
		or: [ self size < aSequenceableCollection size ])
		ifTrue: [ ^ false ].
	aSequenceableCollection
		withIndexDo: [ :each :index | 
			(self at: index) ~= each
				ifTrue: [ ^ false ] ].
	^ true

]

{ #category : '*rowan-tonel-gemstone-kernel' }
SequenceableCollection >> fifth [

	"Answer the fifth element of the receiver.
	Raise an error if there are not enough elements."

	^ self at: 5

]

{ #category : '*rowan-tonel-gemstone-kernel' }
SequenceableCollection >> fourth [

	"Answer the fourth element of the receiver.
	Raise an error if there are not enough elements."

	^ self at: 4

]

{ #category : '*rowan-tonel-gemstone-kernel' }
SequenceableCollection >> pairsCollect: aBlock [ 
	"Evaluate aBlock with my elements taken two at a time, and return an Array with the results"

	^ (1 to: self size // 2) collect:
		[:index | aBlock value: (self at: 2 * index - 1) value: (self at: 2 * index)]
"
#(1 'fred' 2 'charlie' 3 'elmer') pairsCollect:
	[:a :b | b, ' is number ', a printString]
"

]

{ #category : '*rowan-tonel-gemstone-kernel' }
SequenceableCollection >> putOn: aStream [

	self do: [ :each | each putOn: aStream ]

]

{ #category : '*rowan-tonel-gemstone-kernel' }
SequenceableCollection >> sixth [

	"Answer the sixth element of the receiver.
	Raise an error if there are not enough elements."

	^ self at: 6

]

{ #category : '*rowan-tonel-gemstone-kernel' }
SequenceableCollection >> third [

	"Answer the third element of the receiver.
	Raise an error if there are not enough elements."

	^ self at: 3

]

{ #category : '*rowan-tonel-gemstone-kernel' }
SequenceableCollection >> withIndexDo: elementAndIndexBlock [

	"Just like with:do: except that the iteration index supplies the second argument to the block."

	1 to: self size do: [ :index | elementAndIndexBlock value: (self at: index) value: index ]

]

{ #category : '*rowan-tonel-gemstone-kernel' }
SequenceableCollection >> writeStreamPortable [

	^ WriteStreamPortable on: self

]
