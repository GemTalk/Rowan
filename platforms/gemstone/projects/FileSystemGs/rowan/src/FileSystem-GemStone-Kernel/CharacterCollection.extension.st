Extension { #name : 'CharacterCollection' }

{ #category : '*filesystem-gemstone-kernel' }
CharacterCollection >> fillFrom: aCollection with: aBlock [
	"Evaluate aBlock with each of aCollections's elements as the argument.  
	Collect the resulting values into self. Answer self."

	| index |
	index := 0.
	aCollection do: [ :each |
		self at: (index := index + 1) put: (aBlock value: each) ]
]

{ #category : '*filesystem-gemstone-kernel' }
CharacterCollection >> fullName [
	^self
]

{ #category : '*filesystem-gemstone-kernel' }
CharacterCollection class >> crlf [
	"Answer a string containing a carriage return and a linefeed."

	^ self with: Character cr with: Character lf
]
