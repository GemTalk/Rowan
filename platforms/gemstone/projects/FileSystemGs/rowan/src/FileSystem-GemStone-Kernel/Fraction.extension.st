Extension { #name : 'Fraction' }

{ #category : '*FileSystem-GemStone-Kernel' }
Fraction >> isPowerOfTwo [
	^ numerator = 1 and: [ denominator isPowerOfTwo ]
]
