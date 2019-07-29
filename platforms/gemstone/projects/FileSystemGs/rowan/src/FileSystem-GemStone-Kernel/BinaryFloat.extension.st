Extension { #name : 'BinaryFloat' }

{ #category : '*FileSystem-GemStone-Kernel' }
BinaryFloat >> isPowerOfTwo [
	"Return true if the receiver is an integral power of two.
	Floats never return true here."
	^false
]

{ #category : '*FileSystem-GemStone-Kernel' }
BinaryFloat class >> readFrom: aStream ifFail: aBlock [
	"Answer a new Float as described on the stream, aStream."

	^(super readFrom: aStream ifFail: [^aBlock value]) asFloat

]
