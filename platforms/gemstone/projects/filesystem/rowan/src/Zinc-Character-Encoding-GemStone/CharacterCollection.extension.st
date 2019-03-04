Extension { #name : 'CharacterCollection' }

{ #category : '*Zinc-Character-Encoding-GemStone' }
CharacterCollection >> asZnCharacterEncoder [
	"Return a ZnCharacterEncoder instance using the receiver as identifier"

	self flag: 'GemStone/Pharo code switched for research'.

	self isGemStoneUtf8Encoding
		ifTrue: [
			^ ZnCharacterEncoder newForEncoding: self
			" 'UTF-8' asZnCharacterEncoder 
			((self select: [ :each | each isAlphaNumeric ]) asLowercase) = 'utf8' 
				ifFalse: [ self error: 'Only utf8 encoding supported'].
			^ ZnUTF8Encoder new"]
		ifFalse: [
			^ ZnCharacterEncoder newForEncoding: self]
]

{ #category : '*Zinc-Character-Encoding-GemStone' }
CharacterCollection >> encodeWith: encoding [
	"Produce a ByteArray that encodes the receiver, using a specified encoding.
	Encoding is either a ZnCharacterEncoder instance or an identifier for one."
	
	" 'Les élèves français' encodeWith: #utf8 "
	
	^ encoding asZnCharacterEncoder encodeString: self

]

{ #category : '*Zinc-Character-Encoding-GemStone' }
CharacterCollection >> isByteString [

	^false

]

{ #category : '*Zinc-Character-Encoding-GemStone' }
CharacterCollection >> isWideString [
	"Answer whether the receiver is a WideString"
	^false
]

{ #category : '*Zinc-Character-Encoding-GemStone' }
CharacterCollection >> urlDecoded [
	"URL Decode the receiver and return the resulting String.
	This is an encoding where characters that are illegal in a URL are escaped."

    ^ ZnPercentEncoder new decode: self
]

{ #category : '*Zinc-Character-Encoding-GemStone' }
CharacterCollection >> urlEncoded [
	"URL Encode the receiver and return the resulting String.
	This is an encoding where characters that are illegal in a URL are escaped."

    ^ ZnPercentEncoder new encode: self
]

{ #category : '*Zinc-Character-Encoding-GemStone' }
CharacterCollection >> utf8Encoded [
	"Produce a ByteArray encoding the receiver using UTF-8,
	the recommended encoding for Strings, unless you know what you are doing."
	
	" 'Les élèves français' utf8Encoded "
	
	^ self encodeWith: ZnCharacterEncoder utf8
]
