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

{ #category : 'converting' }
ZnUTF8Encoder >> backOnStream: stream [
	"Move back one character on stream"

	[ (stream back bitAnd: 2r11000000) == 2r10000000 ] whileTrue
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

{ #category : 'converting' }
ZnUTF8Encoder >> encodedByteCountFor: character [
	"Return how many bytes are needed to encode character"
		
	^ character asString encodeAsUTF8 size
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
	
	^ string encodeAsUTF8 asByteArray
]

{ #category : 'error handling' }
ZnUTF8Encoder >> error: message [
	^ #ZnInvalidUTF8 signal: message
]

{ #category : 'error handling' }
ZnUTF8Encoder >> errorIllegalContinuationByte [
	^ self error: 'Illegal continuation byte for utf-8 encoding'
]

{ #category : 'error handling' }
ZnUTF8Encoder >> errorOverlong [
	^ self error: 'Overlong utf-8 encoding (non-shortest form)'
]

{ #category : 'accessing' }
ZnUTF8Encoder >> identifier [
	^ #utf8
]

{ #category : 'testing' }
ZnUTF8Encoder >> isSurrogateCodePoint: codePoint [
	"Surrogate Code Points should not be encoded or decoded because they are not Unicode scalar values"
	
	^ codePoint between: 16rD800 and: 16rDFFF
]

{ #category : 'convenience' }
ZnUTF8Encoder >> next: count putAll: string startingAt: offset toStream: stream [
	"Write count characters from string starting at offset to stream."
	
	stream nextPutAll: (string copyFrom: offset to: offset + count - 1) encodeAsUTF8 asByteArray
]

{ #category : 'converting' }
ZnUTF8Encoder >> nextCodePointFromStream: stream [
	"Read and return the next integer code point from stream"

	| code byte next |
	(byte := stream next ifNil: [ ^ self errorIncomplete ]) < 128
		ifTrue: [ ^ byte ].
	(byte bitAnd: 2r11100000) == 2r11000000
		ifTrue: [ 
			code := byte bitAnd: 2r00011111.
			((next := stream next ifNil: [ ^ self errorIncomplete ]) bitAnd: 2r11000000) == 2r10000000
				ifTrue: [ code := (code bitShift: 6) + (next bitAnd: 2r00111111) ]
				ifFalse: [ ^ self errorIllegalContinuationByte ].
			code < 128 ifTrue: [ ^ self errorOverlong ].
			^ code ].
	(byte bitAnd: 2r11110000) == 2r11100000
		ifTrue: [ 
			code := byte bitAnd: 2r00001111.
			2 timesRepeat: [ 
				((next := stream next ifNil: [ ^ self errorIncomplete ]) bitAnd: 2r11000000) == 2r10000000
					ifTrue: [ code := (code bitShift: 6) + (next bitAnd: 2r00111111) ]
					ifFalse: [ ^ self errorIllegalContinuationByte ] ].
			code < 2048 ifTrue: [ ^ self errorOverlong ].
			(self isSurrogateCodePoint: code) ifTrue: [ ^ self errorOutsideRange ].
			code = 65279 "Unicode Byte Order Mark" ifTrue: [ 
				stream atEnd ifTrue: [ ^ self errorIncomplete ].
				^ self nextCodePointFromStream: stream ].
			^ code ].
	(byte bitAnd: 2r11111000) == 2r11110000
		ifTrue: [ 
			code := byte bitAnd: 2r00000111.
			3 timesRepeat: [ 
				((next := stream next ifNil: [ ^ self errorIncomplete ]) bitAnd: 2r11000000) == 2r10000000
					ifTrue: [ code := (code bitShift: 6) + (next bitAnd: 2r00111111) ]
					ifFalse: [ ^ self errorIllegalContinuationByte ] ].
			code < 65535 ifTrue: [ ^ self errorOverlong ].
			^ code ].
	^ self errorIllegalLeadingByte
]

{ #category : 'converting' }
ZnUTF8Encoder >> nextPutCodePoint: codePoint toStream: stream [
	"Write the encoding for Integer code point to stream"

	codePoint < 128 ifTrue: [ 
		^ stream nextPut: codePoint ].
	codePoint < 2048 ifTrue: [ 
		^ stream 
			nextPut: (2r11000000 + (codePoint bitShift: -6)); 
			nextPut: (2r10000000 + (codePoint bitAnd: 2r111111)) ].
	(self isSurrogateCodePoint: codePoint) ifTrue: [ ^ self errorOutsideRange ].
	codePoint < 65536 ifTrue: [ 
		^ stream 
			nextPut: (2r11100000 + (codePoint bitShift: -12));
			nextPut: (2r10000000 + ((codePoint bitShift: -6) bitAnd: 2r111111)); 
			nextPut: (2r10000000 + (codePoint bitAnd: 2r111111)) ].
	codePoint <= self maximumUTFCode ifTrue: [ 
		^ stream 
			nextPut: (2r11110000 + (codePoint bitShift: -18));
			nextPut: (2r10000000 + ((codePoint bitShift: -12) bitAnd: 2r111111));
			nextPut: (2r10000000 + ((codePoint bitShift: -6) bitAnd: 2r111111)); 
			nextPut: (2r10000000 + (codePoint bitAnd: 2r111111)) ].
	^ self errorOutsideRange
]

{ #category : 'convenience' }
ZnUTF8Encoder >> readInto: aCollection startingAt: offset count: requestedCount fromStream: stream [
	"Read requestedCount characters into string starting at offset,
	returning the number read, there could be less available when stream is atEnd"

	| decodedCollection pos |
	pos := stream position.
	decodedCollection := self decodeBytes: stream contents.
	aCollection 
		replaceFrom: offset 
		to: offset + (requestedCount min: decodedCollection size) - 1
		with: decodedCollection
		startingAt: pos + 1.
	stream setToEnd.
	^ 	requestedCount min: decodedCollection size
]
