Extension { #name : 'Character' }

{ #category : '*filesystem-gemstone-kernel' }
Character >> charCode [
	^ self asInteger bitAnd: 4194303
]

{ #category : '*filesystem-gemstone-kernel' }
Character >> isCharacter [

	^ true

]

{ #category : '*filesystem-gemstone-kernel' }
Character >> join: aSequenceableCollection [
	"Append the elements of the argument, aSequenceableCollection, separating them by the receiver."
	"(Character space join: #('Pharo' 'is' 'cool')) >>> 'Pharo is cool'"
	^ self asString join: aSequenceableCollection

]

{ #category : '*filesystem-gemstone-kernel' }
Character >> numberParserDigitalValue [
	self asInteger > 255
		ifTrue: [
			self error: 'Characters > 255 are not yet supported - or something'
			"^ self characterSet digitValueOf: self "].
	^ self class numberParserDigitalValues at: 1 + self asInteger
]

{ #category : '*filesystem-gemstone-kernel' }
Character >> to: other [
	"Answer with a collection in ascii order -- $a to: $z"
	^ (self asciiValue to: other asciiValue) collect:
				[:ascii | Character value: ascii]
]

{ #category : '*filesystem-gemstone-kernel' }
Character class >> digitValue: x [ 
	"Answer the Character whose digit value is x. For example,
	 answer $9 for x=9, $0 for x=0, $A for x=10, $Z for x=35."

	| n |
	n := x asInteger.
	^self withValue: (n < 10 ifTrue: [n + 48] ifFalse: [n + 55])

]

{ #category : '*filesystem-gemstone-kernel' }
Character class >> numberParserDigitalValues [
	"This is to replicate the Pharo implimentation of Charactr class>>initializeDigitalValues"
		
	^{
			Character numberParserDigitalValuesBlock value.
		} first
]

{ #category : '*filesystem-gemstone-kernel' }
Character class >> numberParserDigitalValuesBlock [
	"This is to replicate the Pharo implimentation of Charactr class>>initializeDigitalValues"

	| aBlock|
	aBlock := [ | anArray | 
			"Initialize the well known digit value of ascii characters.
			Note that the DigitValues table is 1-based while ascii values are 0-based, thus the offset+1."
			anArray := Array new: 256 withAll: -1.
			"the digits"
			0 to: 9 do: [:i | anArray at: 48 + i + 1 put: i].
			"the uppercase letters"
			10 to: 35 do: [:i | anArray at: 55 + i + 1 put: i].
			"the lowercase letters"
			10 to: 35 do: [:i | anArray at: 87 + i + 1 put: i].
		anArray].
	^aBlock
]

{ #category : '*filesystem-gemstone-kernel' }
Character class >> to: other [
"Had problems deleting this method. It needs to be removed."
   ^self
]

{ #category : '*filesystem-gemstone-kernel' }
Character class >> value: anInteger [
	"Compatability with Pharo"
	^self withValue: anInteger
]
