Extension { #name : 'SequenceableCollection' }

{ #category : '*tonel-gemstone-kernel' }
SequenceableCollection >> allButLast [

	"Answer a copy of the receiver containing all but the last
	element. Raise an error if there are not enough elements."

	^ self allButLast: 1

]

{ #category : '*tonel-gemstone-kernel' }
SequenceableCollection >> allButLast: n [

	"Answer a copy of the receiver containing all but the last n
	elements. Raise an error if there are not enough elements."

	^ self copyFrom: 1 to: self size - n

]

{ #category : '*tonel-gemstone-kernel' }
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

{ #category : '*tonel-gemstone-kernel' }
SequenceableCollection >> fifth [

	"Answer the fifth element of the receiver.
	Raise an error if there are not enough elements."

	^ self at: 5

]

{ #category : '*tonel-gemstone-kernel' }
SequenceableCollection >> fourth [

	"Answer the fourth element of the receiver.
	Raise an error if there are not enough elements."

	^ self at: 4

]

{ #category : '*tonel-gemstone-kernel' }
SequenceableCollection >> putOn: aStream [

	self do: [ :each | each putOn: aStream ]

]

{ #category : '*tonel-gemstone-kernel' }
SequenceableCollection >> second [

	"Answer the second element of the receiver.
	Raise an error if there are not enough elements."

	^ self at: 2

]

{ #category : '*tonel-gemstone-kernel' }
SequenceableCollection >> sixth [

	"Answer the sixth element of the receiver.
	Raise an error if there are not enough elements."

	^ self at: 6

]

{ #category : '*tonel-gemstone-kernel' }
SequenceableCollection >> sort [

	"Sort this array into ascending order using the '<=' operator."

	^ self sort: [ :a :b | a <= b ]

]

{ #category : '*tonel-gemstone-kernel' }
SequenceableCollection >> third [

	"Answer the third element of the receiver.
	Raise an error if there are not enough elements."

	^ self at: 3

]

{ #category : '*tonel-gemstone-kernel' }
SequenceableCollection >> withIndexDo: elementAndIndexBlock [

	"Just like with:do: except that the iteration index supplies the second argument to the block."

	1 to: self size do: [ :index | elementAndIndexBlock value: (self at: index) value: index ]

]

{ #category : '*tonel-gemstone-kernel' }
SequenceableCollection >> writeStreamPortable [

	^ WriteStreamPortable on: self

]
