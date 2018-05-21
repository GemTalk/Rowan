"
STONWriterTests test serialization.
"
Class {
	#name : 'STONWriterTests',
	#superclass : 'TestCase',
	#category : 'STON-Tests'
}

{ #category : 'Testing' }
STONWriterTests class >> shouldInheritSelectors [
  "Me and my subclasses inherit selectors"

  ^ true
]

{ #category : 'private' }
STONWriterTests >> encodeOnSerialize [
  ^ false
]

{ #category : 'private' }
STONWriterTests >> jsonWriter [
  ^ self writer
    jsonMode: true;
    yourself
]

{ #category : 'private' }
STONWriterTests >> materialize: string [
  | str |
  str := string.
  self encodeOnSerialize
    ifTrue: [ str := string decodeFromUTF8 ].
  ^ self reader
    on: str readStream;
    allowComplexMapKeys: true;
    next
]

{ #category : 'private' }
STONWriterTests >> prettyWriter [
  ^ self writer
    prettyPrint: true;
    yourself
]

{ #category : 'private' }
STONWriterTests >> reader [
  ^ STONReader new
]

{ #category : 'private' }
STONWriterTests >> serialize: anObject [
  | str |
  str := String
    streamContents: [ :stream | 
      self writer
        on: stream;
        nextPut: anObject ].
  self encodeOnSerialize
    ifFalse: [ ^ str ].
  ^ str encodeAsUTF8intoString
]

{ #category : 'private' }
STONWriterTests >> serializeJson: anObject [
  | str |
  str := String
    streamContents: [ :stream | 
      self jsonWriter
        on: stream;
        nextPut: anObject ].
  self encodeOnSerialize
    ifFalse: [ ^ str ].
  ^ str encodeAsUTF8intoString
]

{ #category : 'private' }
STONWriterTests >> serializePretty: anObject [
  | str |
  str := String
    streamContents: [ :stream | 
      self prettyWriter
        on: stream;
        nextPut: anObject ].
  self encodeOnSerialize
    ifFalse: [ ^ str ].
  ^ str encodeAsUTF8intoString
]

{ #category : 'tests' }
STONWriterTests >> testBoolean [
	self assert: (self serialize: true) = 'true'.
	self assert: (self serialize: false) = 'false'
]

{ #category : 'tests' }
STONWriterTests >> testByteArray [
	self assert: (self serialize: #(1 2 3) asByteArray) = 'ByteArray[''010203'']' 

]

{ #category : 'tests' }
STONWriterTests >> testCustomNewline [
	| output lf expectedResult |
	lf := String with: Character lf.
	output := String streamContents: [ :out |
		(STON writer on: out)
			newLine: lf;
			prettyPrint: true;
			nextPut: #( 1 ) ].
	expectedResult := String streamContents: [:out |
		out 
			nextPut: $[;
			lf;
			tab;
			nextPut: $1;
			lf;
			nextPut: $];
			yourself ].
	self 
		assert: output 
		equals: expectedResult

]

{ #category : 'tests' }
STONWriterTests >> testDate [
	| date |
	date := Date newDay: 1 month:  'January' year: 2012.
	self assert: (self serialize: date) = 'Date[''2012-01-01'']'

]

{ #category : 'tests' }
STONWriterTests >> testDateAndTime [
	| dateAndTime |
	dateAndTime := DateAndTime year: 2012 month: 1 day: 1 hour: 6 minute: 30 second: 15 offset: (Duration seconds: 60*60).
	self assert: (self serialize: dateAndTime) = 'DateAndTime[''2012-01-01T06:30:15+01:00'']'

]

{ #category : 'tests' }
STONWriterTests >> testDictionary [
	| collection |
	collection := STON mapClass new at: 1 put: 1; at: 2 put: 2; yourself.
	self assert: (self serialize: collection) = '{1:1,2:2}'.
	self assert: (self serialize: STON mapClass new) = '{}'.
]

{ #category : 'tests' }
STONWriterTests >> testDictionaryWithComplexKeys [
  "order dependent test"

  | collection newCollection |
  collection := STON mapClass new
    at: true put: 1;
    at: #(#'foo') put: 2;
    yourself.
  newCollection := self materialize: (self serialize: collection).
  self assert: newCollection = collection
]

{ #category : 'tests' }
STONWriterTests >> testDoubleQuotedString [
  | string |
  self assert: (self serializeJson: 'foo') = '"foo"'.
  self assert: (self serializeJson: 'FOO') = '"FOO"'.
  self
    assert:
      (self serializeJson: 'élève en Français') = '"\u00E9l\u00E8ve en Fran\u00E7ais"'.
  string := String
    withAll:
      {$".
      $'.
      $\.
      (Character tab).
      (Character cr).
      (Character lf).
      (Character newPage).
      (Character backspace)}.
  self assert: (self serializeJson: string) = '"\"\''\\\t\r\n\f\b"'
]

{ #category : 'tests' }
STONWriterTests >> testEmptyArrayPretty [
	self assert: (self serializePretty: STON listClass new) equals: '[ ]'
]

{ #category : 'tests' }
STONWriterTests >> testEmptyDictionaryPretty [
	self assert: (self serializePretty: STON mapClass new) equals: '{ }'
]

{ #category : 'tests' }
STONWriterTests >> testFloat [
	self assert: (self serialize: 1.5) = '1.5'.
	self assert: (self serialize: 0.0) = '0.0'.
	self assert: (self serialize: -1.5) = '-1.5'.
	self assert: ((self serialize: Float pi) beginsWith:  '3.14159').
	self assert: ((self serialize: 1/3) beginsWith:  '0.333').
	self assert: (self serialize: (10 raisedTo: 100) asFloat) = '1.0e100'.
	self assert: (self serialize: (10 raisedTo: -50) asFloat) = '1.0e-50'.
	self assert: (self serialize: (10 raisedTo: -50) asFloat negated) = '-1.0e-50'.
]

{ #category : 'tests' }
STONWriterTests >> testIdentityDictionary [
	| collection |
	collection := IdentityDictionary new at: 1 put: 1; at: 2 put: 2; yourself.
	self assert: (self serialize: collection) = 'IdentityDictionary{1:1,2:2}'.
	self assert: (self serialize: IdentityDictionary new) = 'IdentityDictionary{}'.
]

{ #category : 'tests' }
STONWriterTests >> testInteger [
	self assert: (self serialize: 1) = '1'.
	self assert: (self serialize: 0) = '0'.
	self assert: (self serialize: -1) = '-1'.
	self assert: (self serialize: 1234567890) = '1234567890'.
	self assert: (self serialize: -1234567890) = '-1234567890'
]

{ #category : 'tests' }
STONWriterTests >> testIsSimpleSymbol [
	self assert: (STON writer isSimpleSymbol: #foo).
	self assert: (STON writer isSimpleSymbol: #az).
	self assert: (STON writer isSimpleSymbol: #AZ).
	self assert: (STON writer isSimpleSymbol: #N0123456789).
	self assert: (STON writer isSimpleSymbol: #foo123).
	self assert: (STON writer isSimpleSymbol: #'Foo/Bar').
	self assert: (STON writer isSimpleSymbol: #'Foo.Bar').
	self assert: (STON writer isSimpleSymbol: #'Foo-Bar').
	self assert: (STON writer isSimpleSymbol: #'Foo_Bar').
	self assert: (STON writer isSimpleSymbol: #foo).
	self deny: (STON writer isSimpleSymbol: #'#^&$%')
]

{ #category : 'tests' }
STONWriterTests >> testList [
	self assert: (self serialize: (STON listClass withAll: #(1 2 3))) = '[1,2,3]'.
	self assert: (self serialize: STON listClass new) = '[]'.
	self assert: (self serialize: (STON listClass withAll: { 1. -1. 0. #foo. 'a b c'. true. false. nil })) = '[1,-1,0,#foo,''a b c'',true,false,nil]'
]

{ #category : 'tests' }
STONWriterTests >> testMap [
  | map ston |
  (map := STON mapClass new)
    at: #'foo' put: 1;
    at: #'bar' put: 2.
  ston := self serialize: map.
  self assert: (ston = '{#foo:1,#bar:2}' or: [ ston = '{#bar:2,#foo:1}' ]).
  self assert: (self serialize: STON mapClass new) = '{}'.
  map removeAllKeys: map keys.
  map at: 'foo bar' put: #'ok'.
  self assert: (self serialize: map) = '{''foo bar'':#ok}'.
  map removeAllKeys: map keys.
  map at: 123 put: 456.
  self assert: (self serialize: map) = '{123:456}'

]

{ #category : 'tests' }
STONWriterTests >> testNil [
	self assert: (self serialize: nil) = 'nil'
]

{ #category : 'tests' }
STONWriterTests >> testNull [
	self assert: (self serializeJson: nil) equals: 'null'
]

{ #category : 'tests' }
STONWriterTests >> testObject [

	self assert: (self serialize: Object new) = 'Object{}'

]

{ #category : 'tests' }
STONWriterTests >> testOrderedCollection [
	| collection |
	collection := OrderedCollection with: 1 with: 2 with: 3.
	self assert: (self serialize: collection) = 'OrderedCollection[1,2,3]'.
	self assert: (self serialize: OrderedCollection new) = 'OrderedCollection[]'.
]

{ #category : 'tests' }
STONWriterTests >> testReferenceCycle [
	| array |
	array := STON listClass with: 1 with: nil.
	array at: 2 put: array.
	self assert: (self serialize: array) = '[1,@1]'.
]

{ #category : 'tests' }
STONWriterTests >> testReferenceSharing [
	| array one |
	one := { #one }.
	array := STON listClass with: one with: one with: one.
	self assert: (self serialize: array) = '[[#one],@2,@2]'.
]

{ #category : 'tests' }
STONWriterTests >> testReferenceSharingError [
	| serializer array one |
	serializer := [ :object | 
		String streamContents: [ :stream |
			STON writer 
				on: stream;
				referencePolicy: #error; 
				nextPut: object ] ].
	one := { #one }.
	array := STON listClass with: one with: one with: one.
	self 
		should: [ (serializer value: array) = '[[#one],[#one],[#one]]' ] 
		raise: STONWriterError

]

{ #category : 'tests' }
STONWriterTests >> testReferenceSharingIgnore [
	| serializer array one |
	serializer := [ :object | 
		String streamContents: [ :stream |
			STON writer 
				on: stream;
				referencePolicy: #ignore; 
				nextPut: object ] ].
	one := { #one }.
	array := STON listClass with: one with: one with: one.
	self assert: (serializer value: array) = '[[#one],[#one],[#one]]'.
]

{ #category : 'tests' }
STONWriterTests >> testRestrictedClassesInJsonMode [
	self should: [ self serializeJson: STONTestUser dummy ] raise: STONWriterError.

]

{ #category : 'tests' }
STONWriterTests >> testString [
  | x string |
  self assert: (self serialize: 'foo') = '''foo'''.
  self assert: (self serialize: 'FOO') = '''FOO'''.
  self
    assert:
      (x := self serialize: 'élève en Français')
        = '''\u00E9l\u00E8ve en Fran\u00E7ais'''.
  self
    assert:
      (x := self serialize: self unicode16TestString)
        =
          '''\u042F \u043C\u043E\u0436\u0443 \u0457\u0441\u0442\u0438 \u0441\u043A\u043B\u043E, \u0456 \u0432\u043E\u043D\u043E \u043C\u0435\u043D\u0456 \u043D\u0435 \u0437\u0430\u0448\u043A\u043E\u0434\u0438\u0442\u044C.'''.
  false
    ifTrue: [ 
      "ambiguous encoding for 32-bit Unicode characters: https://github.com/svenvc/ston/issues/11"
      self
        assert: (x := self serialize: self unicode32TestString) = '''\u2338F''' ].
  string := String
    withAll:
      {$".
      $'.
      $\.
      (Character tab).
      (Character cr).
      (Character lf).
      (Character newPage).
      (Character backspace)}.
  self assert: (self serialize: string) = '''\"\''\\\t\r\n\f\b'''
]

{ #category : 'tests' }
STONWriterTests >> testSymbol [
	self assert: (self serialize: #foo) = '#foo'.
	self assert: (self serialize: #FOO) = '#FOO'.
	self assert: (self serialize: #bytes) = '#bytes'.
	self assert: (self serialize: #'foo.bar') = '#foo.bar'.
	self assert: (self serialize: #'foo-bar') = '#foo-bar'.
	self assert: (self serialize: #'foo_bar') = '#foo_bar'.
	self assert: (self serialize: #'foo/bar') = '#foo/bar'.
	self assert: (self serialize: #'foo bar') = '#''foo bar'''.
	self assert: (self serialize: #foo123) = '#foo123'.
]

{ #category : 'tests' }
STONWriterTests >> testSymbolAsString [
	self assert: (self serializeJson: #foo) = '"foo"'.
	self assert: (self serializeJson: #'FOO') = '"FOO"'.

]

{ #category : 'tests' }
STONWriterTests >> testTime [
	| time |
	time := Time fromSeconds: (6 * 60 *60) + (30 *60) + 15.
	self assert: (self serialize: time) = 'Time[''06:30:15'']'.

]

{ #category : 'tests' }
STONWriterTests >> testUser [
	| user |
	(user := STONTestUser new)
		username: 'john@foo.com';
		password: 'secret1'.
	self 
		assert: (self serialize: user)
		equals: 'TestUser{#username:''john@foo.com'',#password:''secret1'',#enabled:true}'
]

{ #category : 'tests' }
STONWriterTests >> testUser2 [
	| user |
	(user := STONTestUser2 new)
		username: 'john@foo.com';
		password: 'secret1'.
	self 
		assert: (self serialize: user)
		equals: 'TestUser2{#username:''john@foo.com'',#password:''secret1'',#enabled:true}'
]

{ #category : 'tests' }
STONWriterTests >> testUser3Nil [
	| user |
	user := STONTestUser3 new.
	self 
		assert: (self serialize: user) 
		equals: 'TestUser3{#username:nil,#password:nil,#enabled:true}'
]

{ #category : 'tests' }
STONWriterTests >> testUserNil [
	| user |
	user := STONTestUser new.
	self assert: (self serialize: user) equals: 'TestUser{#enabled:true}'
]

{ #category : 'private' }
STONWriterTests >> unicode16TestString [
  "Cannot embed Unicode16 or Unicode32 strings in 2.x methods"

  ^ 'Ð¯ Ð¼Ð¾Ð¶Ñ ÑÑÑÐ¸ ÑÐºÐ»Ð¾, Ñ Ð²Ð¾Ð½Ð¾ Ð¼ÐµÐ½Ñ Ð½Ðµ Ð·Ð°ÑÐºÐ¾Ð´Ð¸ÑÑ.'
    decodeFromUTF8
]

{ #category : 'private' }
STONWriterTests >> unicode32TestString [
  "Cannot embed Unicode16 or Unicode32 strings in 2.x methods"

  ^ 'ð£' decodeFromUTF8
]

{ #category : 'private' }
STONWriterTests >> writer [
  ^ STONWriter new
]
