"
I am ZnCharacterEncoder, I encode and decode Character objects to and from a binary stream.
I am an abstract class with following protocol:

#nextFromStream:
#nextPut:toStream:
#encodedByteCountFor:
#backOnStream:

The first two are compatible with TextConverter and subclasses.

I add some convenience methods:

#encodeString:
#decodeBytes:
#encodedByteCountForString:

Contrary to older encoders, I work strictly from strings to bytes and vice versa and I will throw errors instead of silently ignoring them.

I also implement optimized bulk operations:

#next:putAll:startingAt:toStream:
#readInto:startingAt:count:fromStream:

Additionally, I can encode Integer code points to a binary stream as well as read Integer code points from a binary stream. This is in a sense a more fundamental operation that avoids instanciating Character objects.

#nextCodePointFromStream:
#nextPutCodePoint:toStream:
#encodedByteCountForCodePoint:

#decodeAsCodePoints:
#encodeCodePoints:
#encodedByteCountForCodePoints:

Part of Zinc HTTP Components.
"
Class {
	#name : 'ZnCharacterEncoder',
	#superclass : 'Object',
	#category : 'Zinc-Character-Encoding-Core'
}

{ #category : 'accessing' }
ZnCharacterEncoder class >> canonicalEncodingIdentifier: string [
	^ (string select: [ :each | each isAlphaNumeric ]) asLowercase
]

{ #category : 'accessing' }
ZnCharacterEncoder class >> handlesEncoding: string [
	"Return true when my instances handle the encoding described by string"
	
	self subclassResponsibility: #handlesEncoding:
]

{ #category : 'accessing' }
ZnCharacterEncoder class >> knownEncodingIdentifiers [
	"Return a collection of all known encoding identifiers in the system"
	
	self = ZnCharacterEncoder ifFalse: [ ^ #() ].
	^ Array streamContents: [ :all |
		self allSubclasses do: [ :subClass |
			all nextPutAll: subClass knownEncodingIdentifiers ] ]
]

{ #category : 'instance creation' }
ZnCharacterEncoder class >> newForEncoding: string [
	"Return a new character encoder object for an encoding described by string.
	Search for a subclass that handles it and delegate (subclassResponsibility)."
	
	| concreteSubclass |
	concreteSubclass := self allSubclasses 
		detect: [ :each | each handlesEncoding: string ] 
		ifNone: [ ^ self error: 'The ', string printString, ' is not currently supported.' ].
	^ concreteSubclass newForEncoding: string
]

{ #category : 'converting' }
ZnCharacterEncoder >> asZnCharacterEncoder [
	^ self
]

{ #category : 'converting' }
ZnCharacterEncoder >> backOnStream: stream [
	"Move back one character on stream, assuming stream understands #back"
	
	self subclassResponsibility: #backOnStream:
]

{ #category : 'convenience' }
ZnCharacterEncoder >> decodeAsCodePoints: bytes [
	"Decode bytes and return the resulting code points"
	
	self subclassResponsibility: #decodeAsCodePoints:
]

{ #category : 'convenience' }
ZnCharacterEncoder >> encodeCodePoints: codePoints [
	"Encode codePoints and return the resulting byte array"
	
	^ self subclassResponsibility: #encodeCodePoints:
]

{ #category : 'converting' }
ZnCharacterEncoder >> encodedByteCountFor: character [
	"Return how many bytes are needed to encode character"
	
	"We should use #codePoint but #asInteger is faster"
	
	^ self encodedByteCountForCodePoint: character asInteger
]

{ #category : 'convenience' }
ZnCharacterEncoder >> encodedByteCountForCodePoints: codePoints [
	"Return the exact number of bytes it would take to encode codePoints as a byte array"

	^self subclassResponsibility: #encodedByteCountForCodePoints:
]

{ #category : 'convenience' }
ZnCharacterEncoder >> encodedByteCountForString: string [
	"Return the exact number of bytes it would take to encode string as a byte array"

	^self subclassResponsibility: #encodedByteCountForCodePoints:
]

{ #category : 'convenience' }
ZnCharacterEncoder >> next: count putAll: string startingAt: offset toStream: stream [
	"Write count characters from string starting at offset to stream."
	
	offset to: offset + count - 1 do: [ :index | 
		self nextPut: (string at: index) toStream: stream ]
]

{ #category : 'converting' }
ZnCharacterEncoder >> nextCodePointFromStream: stream [
	"Read and return the next integer code point from stream"
	
	self subclassResponsibility: #nextCodePointFromStream:
]

{ #category : 'converting' }
ZnCharacterEncoder >> nextFromStream: stream [
	"Read and return the next character from stream"
	
	"We should use #codePoint: but #value: is faster"
	
	^ Character value: (self nextCodePointFromStream: stream)
]

{ #category : 'converting' }
ZnCharacterEncoder >> nextPut: character toStream: stream [
	"Write the encoding for character to stream"
	
	"We should use #codePoint but #asInteger is faster"
	
	self nextPutCodePoint: character asInteger toStream: stream
]
