Extension { #name : 'Random' }

{ #category : '*FileSystem-GemStone-Kernel' }
Random >> nextInt: anInteger [
	"Answer a random integer in the interval [1, anInteger].
	Handle large numbers too (for cryptography)."
	
	"This is Pharo code that is not implemented at the moment"

	"anInteger strictlyPositive ifFalse: [ self error: 'Range must be positive' ].
	anInteger asFloat isInfinite
		ifTrue: [^(self privateNextValue asFraction * anInteger) truncated + 1]."

	^ (self next * anInteger) truncated + 1

]
