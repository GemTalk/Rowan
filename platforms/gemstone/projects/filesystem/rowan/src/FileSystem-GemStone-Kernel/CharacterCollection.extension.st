Extension { #name : 'CharacterCollection' }

{ #category : '*filesystem-gemstone-kernel' }
CharacterCollection >> asZnCharacterEncoder [
	"Return a ZnCharacterEncoder instance using the receiver as identifier"
	
	" 'UTF-8' asZnCharacterEncoder "
	
	((self select: [ :each | each isAlphaNumeric ]) asLowercase) = 'utf8' ifFalse: [ self error: 'Only utf8 encoding supported'].
	^ ZnUTF8Encoder new

]

{ #category : '*filesystem-gemstone-kernel' }
CharacterCollection >> encodeWith: encoding [
	"Produce a ByteArray that encodes the receiver, using a specified encoding.
	Encoding is either a ZnCharacterEncoder instance or an identifier for one."
	
	" 'Les élèves français' encodeWith: #utf8 "
	
	^ encoding asZnCharacterEncoder encodeString: self

]

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
CharacterCollection >> isByteString [

	^false

]

{ #category : '*filesystem-gemstone-kernel' }
CharacterCollection >> urlDecoded [
	"URL Decode the receiver and return the resulting String.
	This is an encoding where characters that are illegal in a URL are escaped."

    ^ ZnPercentEncoder new decode: self
]

{ #category : '*filesystem-gemstone-kernel' }
CharacterCollection >> urlEncoded [
	"URL Encode the receiver and return the resulting String.
	This is an encoding where characters that are illegal in a URL are escaped."

    ^ ZnPercentEncoder new encode: self
]

{ #category : '*filesystem-gemstone-kernel' }
CharacterCollection >> utf8Encoded [
	"Produce a ByteArray encoding the receiver using UTF-8,
	the recommended encoding for Strings, unless you know what you are doing."
	
	" 'Les élèves français' utf8Encoded "
	
	^ self encodeWith: ZnCharacterEncoder utf8
]
