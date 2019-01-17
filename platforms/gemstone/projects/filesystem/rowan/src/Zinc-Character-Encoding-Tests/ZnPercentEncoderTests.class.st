Class {
	#name : 'ZnPercentEncoderTests',
	#superclass : 'TestCase',
	#category : 'Zinc-Character-Encoding-Tests'
}

{ #category : 'tests' }
ZnPercentEncoderTests >> testDecodePlusAsSpace [
	| encoder |
	encoder := ZnPercentEncoder new.
	self assertString: (encoder decode: '+') equalsString: ' '.
	self assert: encoder decodePlusAsSpace.
	encoder decodePlusAsSpace: false.
	self assertString: (encoder decode: '+') equalsString: '+'.
	self deny: encoder decodePlusAsSpace
]

{ #category : 'tests' }
ZnPercentEncoderTests >> testDecodingErrors [
	| encoder |
	encoder := ZnPercentEncoder new.
	self should: [ encoder decode: 'foo%%bar' ] raise: ZnCharacterEncodingError.
	self should: [ encoder decode: 'fooçbar' ] raise: ZnCharacterEncodingError.
	self should: [ encoder decode: 'foo%' ] raise: ZnCharacterEncodingError.
	self should: [ encoder decode: '%XX' ] raise: ZnCharacterEncodingError.
	self should: [ encoder decode: 'foo%F' ] raise: ZnCharacterEncodingError
]

{ #category : 'tests' }
ZnPercentEncoderTests >> testLeadingZero [
	| encoder |
	encoder := ZnPercentEncoder new.
	self assertString: (encoder encode: 'foo', Character tab asString, 'bar') equalsString: 'foo%09bar'.
	self assertString: (encoder decode: 'foo%09bar') equalsString: 'foo', Character tab asString, 'bar'.
	self assertString: (encoder encode: 'foo', Character lf asString, 'bar') equalsString: 'foo%0Abar'.
	self assertString: (encoder decode: 'foo%0Abar') equalsString: 'foo', Character lf asString, 'bar'
]

{ #category : 'tests' }
ZnPercentEncoderTests >> testNonAscii [
	| encoder |
	encoder := ZnPercentEncoder new.
	self 
		assertString: encoder characterEncoder 
		equalsString: (ZnCharacterEncoder newForEncoding: 'utf-8').
	self 
		assertString: (encoder encode: 'élève en Français') 
		equalsString: '%C3%A9l%C3%A8ve%20en%20Fran%C3%A7ais'.
	self 
		assertString: (encoder decode: '%C3%A9l%C3%A8ve%20en%20Fran%C3%A7ais') 
		equalsString: 'élève en Français'
]

{ #category : 'tests' }
ZnPercentEncoderTests >> testSimple [
	| encoder |
	encoder := ZnPercentEncoder new.
	self assertString: (encoder encode: 'foo bar') equalsString: 'foo%20bar'.
	self assertString: (encoder decode: 'foo%20bar') equalsString: 'foo bar'.
	self assertString: (encoder encode: '') equalsString: ''.
	self assertString: (encoder decode: '') equalsString: ''.
	self assertString: (encoder decode: 'foo%25bar') equalsString: 'foo%bar'.
	self assertString: (encoder decode: 'foo+bar') equalsString: 'foo bar'
]

{ #category : 'tests' }
ZnPercentEncoderTests >> testStringUrlDecoded [
	self assertString: ('foo%20bar' urlDecoded) equalsString: 'foo bar'
]
