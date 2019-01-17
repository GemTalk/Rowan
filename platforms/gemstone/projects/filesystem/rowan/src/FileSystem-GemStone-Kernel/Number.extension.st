Extension { #name : 'Number' }

{ #category : '*FileSystem-GemStone-Kernel' }
Number >> asNumber [
	^self
]

{ #category : '*FileSystem-GemStone-Kernel' }
Number >> printStringBase: base [
	^ String streamContents:
		[:strm | self printOn: strm base: base]
]

{ #category : '*FileSystem-GemStone-Kernel' }
Number class >> readFrom: stringOrStream ifFail: aBlock [
	"Answer a number as described on aStream.  The number may
	be any accepted Smalltalk literal Number format.
	It can include a leading radix specification, as in 16rFADE.
	It can as well be NaN, Infinity or -Infinity for conveniency.
	If input does not represent a valid number, then execute fail block
	and leave the stream positioned before offending character"
	
	^(NumberParser on: stringOrStream) failBlock: aBlock; nextNumber

]

{ #category : '*FileSystem-GemStone-Kernel' }
Number class >> squeezeNumberOutOfString: stringOrStream [
	"Try and find a number in this string. First, look if the string 
	starts with a number. Then, see if it ends with a number. Then,
	remove a character from the front and see if the remaining 
	string makes a number. Repeat the process until no characters
	are left or the number has been found. As soon as a number is
	found, it is returned. Otherwise, the method fails."
	^ NumberParser squeezeNumberOutOfString: stringOrStream
]
