Class {
	#name : 'ZnResourceMetaUtilsTests',
	#superclass : 'TestCase',
	#category : 'Zinc-Resource-Meta-Tests'
}

{ #category : 'tests' }
ZnResourceMetaUtilsTests >> testDecodePercent [
	self assertString: (ZnResourceMetaUtils decodePercent: 'foo%20bar') equalsString: 'foo bar'.
	self should: [ ZnResourceMetaUtils decodePercent: 'foo%%bar' ] raise: ZnCharacterEncodingError
]

{ #category : 'tests' }
ZnResourceMetaUtilsTests >> testQueryParsing [
	| string fields |
	string := 'foo=100&x=', (ZnPercentEncoder new encode: '/a b').
	fields := ZnResourceMetaUtils parseQueryFrom: string readStream.
	self assert: (fields at: 'foo') = '100'.
	self assert: (fields at: 'x') = '/a b'.
]

{ #category : 'tests' }
ZnResourceMetaUtilsTests >> testQueryWriting [
	| fields string |
	(fields := Dictionary new)
		at: 'foo' put: '100';
		at: 'bar' put: 'a b'.
	string := String streamContents: [ :stream | 
		ZnResourceMetaUtils writeQueryFields: fields on: stream ].
	self assert: (string includesSubstring: 'foo=1').
	self assert: (string includesSubstring: 'bar=a%20b').
]
