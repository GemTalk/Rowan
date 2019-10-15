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
Integer >> humanReadableSIByteSize [
	^ String streamContents: [ :s|
		self humanReadableSIByteSizeOn: s ]
]

{ #category : '*filesystem-gemstone-kernel' }
Integer >> humanReadableSIByteSizeOn: s [
	| exponent base |
	"Print a string with an SI binary unit represation of myself."
	base := 1000.
	self < base
		ifTrue: [ ^ s print: self; space; nextPut: $B ].
	exponent := (self log / base log) asInteger.
	(self / (base ** exponent)) printOn: s showingDecimalPlaces: 2.
	s 
		space;
		nextPut: ('kMGTPE' at: exponent);
		nextPut: $B.
]
