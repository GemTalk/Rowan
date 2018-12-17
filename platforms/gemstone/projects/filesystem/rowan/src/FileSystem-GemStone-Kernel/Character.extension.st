Extension { #name : 'Character' }

{ #category : '*filesystem-gemstone-kernel' }
Character >> isCharacter [

	^ true

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
Character class >> to: other [
"Had problems deleting this method. It needs to be removed."
   ^self
]

{ #category : '*filesystem-gemstone-kernel' }
Character class >> value: anInteger [
	"Compatability with Pharo"
	^self withValue: anInteger
]
