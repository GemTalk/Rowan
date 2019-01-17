"
I am ZnPositionableReadStream.
I am polymorphic with (the most used/important methods of) ReadStream and PositionableStream.

I wrap another read stream and store the elements that I read in a sliding circular buffer so that I am able to go back to any position inside that buffer. 

Essentially, I implement #position and #position: to be used to back out of reading ahead.

Note that the size of my buffer limits how far I can go backwards. A SubscriptOutOfBounds exception will be signalled when an attempt is made to go too far backwards.

The index returned by #position should be considered abstract, without concrete meaning, but it is currently implemented as the count of elements read by #next on the wrapped stream. On a simple stream over an in memory collection, that will be equivalent to an integer index into that collection. But on network streams or streams that were already further along, this will no longer be the case.

The most elementary example of my capabilities can be seen in my implementation of #peek. See also the unit tests #testPlainExcursion and #testSearch

Of course, backing out of an excursion is only possible within the window of the buffer size.

Implementation

- stream <ReadStream> the read stream that I wrap and add positioning to
- buffer <String|ByteArray> sliding, circular buffer
- index <PositiveInteger> zero based index into buffer, where next will be stored
- count <PositiveInteger> number of next operations done on wrapped stream
- delta <PositiveInteger> number of positions that I was moved backwards

The real core methods are #next, #atEnd, #position and #position: and are used to implement the rest.

Part of Zinc HTTP Components
"
Class {
	#name : 'ZnPercentEncoder',
	#superclass : 'Object',
	#instVars : [
		'characterEncoder',
		'safeSet',
		'decodePlusAsSpace'
	],
	#category : 'Zinc-Character-Encoding-Core'
}

{ #category : 'accessing' }
ZnPercentEncoder class >> rfc3986UnreservedCharacters [
	"Return the unreserved characters according to RFC 3986 section 2.3.
	This is the most narrow safe set to be used in a better safe than sorry approach."

	^ 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789-_.~'
]

{ #category : 'accessing' }
ZnPercentEncoder >> characterEncoder [
	"Return the character encoder that I currently use.
	If not set, I will default to using UTF-8."

	self flag: 'GemStone/Pharo code switched for research'.

	((SessionTemps current at: #ZnEncoding otherwise: #GemStone) = #GemStone)
		ifTrue: [
			self flag: 'TODO: In Pharo, ZnDefaultCharacterEncoder is a DynamicVariable. This must be handled differently in Gemstone'.
			^ characterEncoder ifNil: [ characterEncoder := ZnCharacterEncoder utf8 ] ]
		ifFalse: [
			^ characterEncoder ifNil: [ characterEncoder := ZnDefaultCharacterEncoder value ] ]
]

{ #category : 'initialize-release' }
ZnPercentEncoder >> characterEncoder: object [
	"Set the character encoding to use to object."
	
	characterEncoder := object
]

{ #category : 'converting' }
ZnPercentEncoder >> decode: string [
	"Decode a percent encoded string and return the resulting byte array.
	After percent decoding I will use my character encoder to convert byte values to characters."

	| bytes stringStream |
	stringStream := string readStream.
	bytes := ByteArray streamContents: [ :byteStream | 
		self decode: stringStream to: byteStream ].
	^ self characterEncoder decodeBytes: bytes
]

{ #category : 'converting' }
ZnPercentEncoder >> decode: stringStream to: byteStream [
	| char |
	self decodePlusAsSpace.
	[ stringStream atEnd ]
		whileFalse: [ 
			((char := stringStream next) == $+ and: [ decodePlusAsSpace ])
				ifTrue: [ byteStream nextPut: 32 ]
				ifFalse: [ 
					char == $%
						ifTrue: [ byteStream nextPut: (self readHexFrom: stringStream) ]
						ifFalse: [ 
							char charCode < 128
								ifTrue: [ byteStream nextPut: char charCode ]
								ifFalse: [ self errorAsciiCharacterExpected ] ] ] ]
]

{ #category : 'accessing' }
ZnPercentEncoder >> decodePlusAsSpace [
	"Return if $+ on input should be decoded as Character space.
	This is normally only done application/x-www-form-urlencoded data,
	but is is on by default anyway."
	
	^ decodePlusAsSpace ifNil: [ decodePlusAsSpace := true ]
]

{ #category : 'initialize-release' }
ZnPercentEncoder >> decodePlusAsSpace: boolean [
	"When boolean is true, $+ on input will be decoded as Character space.
	Else $+ is treated as a normal character, filtered by the safe set.
	This is normally only done application/x-www-form-urlencoded data,
	but is is on by default anyway."
	
	decodePlusAsSpace := boolean
]

{ #category : 'converting' }
ZnPercentEncoder >> encode: string [
	"Encode string using percent encoding and return the resulting string.
	I will use my character encoder to convert string to bytes and then
	percent encode all byte values that are not in my safe set."

	^ String streamContents: [ :stream | 
		self encode: string readStream to: stream ]
]

{ #category : 'converting' }
ZnPercentEncoder >> encode: readStream to: writeStream [
	| bytes buffer byte |
	buffer := (bytes := ByteArray new: 4) writeStream.
	self safeSet; characterEncoder.
	[ readStream atEnd ]
		whileFalse: [ 
			buffer reset.
			characterEncoder nextPut: readStream next toStream: buffer.
			1 to: buffer position do: [ :index | 
				(safeSet includes: (byte := bytes at: index))
					ifTrue: [ writeStream nextPut: byte asCharacter ]
					ifFalse: [ 
						writeStream nextPut: $%.
						self writeHex: byte to: writeStream ] ] ]
]

{ #category : 'converting' }
ZnPercentEncoder >> errorAsciiCharacterExpected [
	ZnCharacterEncodingError signal: 'ASCII character expected'
]

{ #category : 'error handling' }
ZnPercentEncoder >> errorHexDigitExpected [
	ZnCharacterEncodingError signal: 'hex digit expected'
]

{ #category : 'private' }
ZnPercentEncoder >> readHexFrom: stream [
	| first second |
	(stream atEnd not and: [ (first := stream next numberParserDigitalValue) between: 0 and: 15 ])
		ifFalse: [ self errorHexDigitExpected ].
	(stream atEnd not and: [ (second := stream next numberParserDigitalValue) between: 0 and: 15 ])
		ifFalse: [ self errorHexDigitExpected ].
	^ (first << 4) + second
]

{ #category : 'accessing' }
ZnPercentEncoder >> safeSet [
	"Return the safe set of characters that I will not encode, as a byte array.
	If not set, I will default to the most commonly used safe set"
	
	^ safeSet ifNil: [ safeSet := self class rfc3986UnreservedCharacters asByteArray ]
]

{ #category : 'initialize-release' }
ZnPercentEncoder >> safeSet: string [
	"Set my safe set to be the characters in string, which I will convert to bytes"
	
	safeSet := string asByteArray
]

{ #category : 'private' }
ZnPercentEncoder >> writeHex: integer to: stream [
	integer printOn: stream base: 16 length: 2 padded: true
]
