Extension { #name : 'Integer' }

{ #category : '*filesystem-gemstone-kernel' }
Integer >> << shiftAmount [
	"left shift"
	
	shiftAmount < 0 ifTrue: [self error: 'negative arg'].
	^ self bitShift: shiftAmount

]

{ #category : '*filesystem-gemstone-kernel' }
Integer >> >> shiftAmount [
	"right shift"
	
	shiftAmount < 0 ifTrue: [self error: 'negative arg'].
	^ self bitShift: 0 - shiftAmount

]

{ #category : '*filesystem-gemstone-kernel' }
Integer >> atRandom [
	"Answer a random integer from 1 to self.  This implementation uses a
	shared generator. Heavy users should their own implementation or use
	Interval>atRandom: directly."

	self = 0 ifTrue: [ ^0 ].
	self < 0 ifTrue: [ ^self negated atRandom negated ].
	^ self atRandom: (Random seed: self)
]

{ #category : '*filesystem-gemstone-kernel' }
Integer >> atRandom: aGenerator [
	"Answer a random integer from 1 to self picked from aGenerator."

	^ aGenerator nextInt: self
]

{ #category : '*filesystem-gemstone-kernel' }
Integer >> digitAt: n [
	"Answer the value of an apparent byte-indexable field in the receiver,
	 analogous to the large integers, which are organized as bytes."

	n = 1
		ifTrue: [ 
			"Negate carefully in case the receiver is SmallInteger minVal"
			^ self < 0
				ifTrue: [ -256 - self bitAnd: 255 ]
				ifFalse: [ self bitAnd: 255 ] ].
	^ self < 0
		ifTrue: [ (-256 - self bitShift: -8) + 1 digitAt: n - 1 ]
		ifFalse: [ (self bitShift: 8 - (n bitShift: 3)) bitAnd: 255 ]

]

{ #category : '*filesystem-gemstone-kernel' }
Integer >> isPowerOfTwo [
	"Return true if the receiver is an integral power of two."
	^ self ~= 0 and: [(self bitAnd: self-1) = 0]
]

{ #category : '*filesystem-gemstone-kernel' }
Integer >> numberOfDigitsInBase: b [ 
	"Return how many digits are necessary to print this number in base b.
	This does not count any place for minus sign, radix prefix or whatever.
	Note that this algorithm may cost a few operations on LargeInteger."

	| nDigits q total |
	self negative ifTrue: [^self negated numberOfDigitsInBase: b].
	self < b ifTrue: [^1].
	b isPowerOfTwo ifTrue: [^self highBit + b highBit - 2 quo: b highBit - 1].
	
	"A conversion from base 2 to base b has to be performed.
	This algorithm avoids Float computations like (self log: b) floor + 1,
	1) because they are inexact
	2) because LargeInteger might overflow
	3) because this algorithm might be cheaper than conversion"

	q := self.
	total := 0.
	["Make an initial nDigits guess that is lower than or equal to required number of digits"
	nDigits := b = 10
		ifTrue: [((q highBit - 1) * 1233 >> 12) + 1. "This is because (2 log)/(10 log)*4096 is slightly greater than 1233"]
		ifFalse: [q highBit quo: b highBit].
	total := total + nDigits.
	
	"See how many digits remains above these first nDigits guess"
	(q := q quo: (b raisedToInteger: nDigits)) < b] whileFalse.
	^q = 0
		ifTrue: [total]
		ifFalse: [total + 1]
]

{ #category : '*filesystem-gemstone-kernel' }
Integer >> print: positiveNumberString on: aStream prefix: prefix length: minimum padded: zeroFlag [
	| padLength |
	padLength := minimum - positiveNumberString size - prefix size.
	padLength > 0
		ifTrue: [zeroFlag
				ifTrue: [aStream nextPutAll: prefix; nextPutAll: (String new: padLength withAll: $0)]
				ifFalse: [aStream nextPutAll: (String new: padLength withAll: Character space); nextPutAll: prefix]]
		ifFalse: [aStream nextPutAll: prefix].
	aStream nextPutAll: positiveNumberString
]

{ #category : '*filesystem-gemstone-kernel' }
Integer >> printOn: aStream base: base length: minimum padded: zeroFlag [
	| prefix |
	
	prefix := self negative ifTrue: ['-'] ifFalse: [ '' ].
	
	self print: (self abs printStringBase: base) on: aStream prefix: prefix length: minimum padded: zeroFlag
]

{ #category : '*FileSystem-GemStone-Kernel' }
Integer class >> readFrom: aStringOrStream ifFail: aBlock [
	"Answer an instance of one of the concrete subclasses if Integer. 
	Initial minus sign accepted.
	Imbedded radix specifiers not allowed;  use Number 
	class readFrom: for that.
	Execute aBlock if there are no digits."

	^(NumberParser on: aStringOrStream) nextIntegerBase: 10 ifFail: aBlock

]
