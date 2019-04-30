Extension { #name : 'PositionableStreamPortable' }

{ #category : '*filesystem-gemstone-kernel' }
PositionableStreamPortable >> isBinary [
	"Return true if the receiver is a binary byte stream"
	^collection class == ByteArray

]

{ #category : '*filesystem-gemstone-kernel' }
PositionableStreamPortable >> nextInto: aCollection [
	"Read the next elements of the receiver into aCollection.
	Return aCollection or a partial copy if less than aCollection
	size elements have been read."
	^self next: aCollection size into: aCollection startingAt: 1.
]
