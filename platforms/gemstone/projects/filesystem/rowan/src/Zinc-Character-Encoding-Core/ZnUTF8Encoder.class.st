"
I am the GemStone/S implementation of ZnUTF8Encoder.
I implement the variable length UTF-8 encoding and decoding of Unicode according to RFC 3629.

Wikipedia reference http://en.wikipedia.org/wiki/UTF-8

Part of Zinc HTTP Components.

I use the ICU library to encode strings and decode ByteArray and Utf8 instances ... 
"
Class {
	#name : 'ZnUTF8Encoder',
	#superclass : 'ZnCharacterEncoder',
	#classVars : [
		'Default'
	],
	#category : 'Zinc-Character-Encoding-Core'
}

{ #category : 'accessing' }
ZnUTF8Encoder class >> default [
	"Return a cached instance of the most commonly used encoder,
	which is faster than going via #newForEncoding: that does a subclass search"
	
	^ Default ifNil: [ Default := self new ]
]

{ #category : 'accessing' }
ZnUTF8Encoder class >> handlesEncoding: string [
	"Return true when my instances handle the encoding described by string"
	
	^ (self canonicalEncodingIdentifier: string) = 'utf8'
]

{ #category : 'accessing' }
ZnUTF8Encoder class >> knownEncodingIdentifiers [
	^ #( utf8 )
]

{ #category : 'instance creation' }
ZnUTF8Encoder class >> newForEncoding: string [
	"No further parametrization needed"
	
	^ self new
]

{ #category : 'convenience' }
ZnUTF8Encoder >> decodeAsCodePoints: bytes [
	"Decode bytes and return the resulting code points"

	| ar |
	ar := {}.
	bytes decodeFromUTF8 do: [:char | ar add: char codePoint ].
	^ ar
]

{ #category : 'convenience' }
ZnUTF8Encoder >> decodeBytes: bytes [
	"Decode bytes and return the resulting string"

	^ bytes decodeFromUTF8
]

{ #category : 'convenience' }
ZnUTF8Encoder >> encodeCodePoints: codePoints [
	"Encode codePoints and return the resulting byte array"
	
	^ codePoints asByteArray asUnicodeString encodeAsUTF8
]

{ #category : 'convenience' }
ZnUTF8Encoder >> encodedByteCountForCodePoints: codePoints [
	"Return the exact number of bytes it would take to encode codePoints as a byte array"

	^ (self encodeCodePoints: codePoints) size
]

{ #category : 'convenience' }
ZnUTF8Encoder >> encodedByteCountForString: string [
	"Return the exact number of bytes it would take to encode string as a byte array"

	^ (self encodeString: string) size
]

{ #category : 'convenience' }
ZnUTF8Encoder >> encodeString: string [
	"Encode string and return the resulting Utf8 instance"
	
	^ string encodeAsUTF8
]

{ #category : 'accessing' }
ZnUTF8Encoder >> identifier [
	^ #utf8
]

{ #category : 'converting' }
ZnUTF8Encoder >> nextCodePointFromStream: stream [
	"Read and return the next integer code point from stream"

	self error: 'not currently supported'
]

{ #category : 'converting' }
ZnUTF8Encoder >> nextPutCodePoint: codePoint toStream: stream [
	"Write the encoding for Integer code point to stream"

	^ stream nextPutAll: codePoint asString encodeAsUTF8
]

{ #category : 'convenience' }
ZnUTF8Encoder >> readInto: aCollection startingAt: offset count: requestedCount fromStream: stream [
	"Read requestedCount characters into string starting at offset,
	returning the number read, there could be less available when stream is atEnd"

	"For the time being, we are expecting that the requestedCount is ALWAYS greater than the
		size of stream ... we cannot encode/decode partial streams"

	| decodedCollection |
	offset > 1
		ifTrue: [ self error: 'decode operation not supported for large collections' ].
	
	decodedCollection := self decodeBytes: stream contents.
	aCollection 
		replaceFrom: offset 
		to: offset + (requestedCount min: decodedCollection size) - 1
		with: decodedCollection
		startingAt: 1.
	stream setToEnd.
	^ 	requestedCount min: decodedCollection size
]
