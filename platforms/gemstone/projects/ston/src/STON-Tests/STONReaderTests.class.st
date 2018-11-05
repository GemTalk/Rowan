"
STONReaderTests test materialization.

"
Class {
	#name : 'STONReaderTests',
	#superclass : 'TestCase',
	#category : 'STON-Tests'
}

{ #category : 'Testing' }
STONReaderTests class >> shouldInheritSelectors [
  "Me and my subclasses inherit selectors"

  ^ true
]

{ #category : 'private' }
STONReaderTests >> materialize: string [
	^ STON reader 
		on: string readStream;
		next
]

{ #category : 'private' }
STONReaderTests >> sumOf: aCollection [
 | sum sample |
  sample := aCollection detect: [:each | true ].
  sum := aCollection inject: sample into: [ :accum :each | accum + each ].
  ^ sum - sample

]

{ #category : 'tests' }
STONReaderTests >> testBoolean [
	self assert: (self materialize: 'true') = true.
	self assert: (self materialize: 'false') = false
]

{ #category : 'tests' }
STONReaderTests >> testByteArray [
	self assert: (self materialize: 'ByteArray[''010203'']') = #(1 2 3) asByteArray

]

{ #category : 'tests' }
STONReaderTests >> testCharacter [
	self assert: (self materialize: 'Character[''A'']') == $A.
]

{ #category : 'tests' }
STONReaderTests >> testDate [
	| date |
	date := Date newDay: 1 month:  'January' year: 2012.
	self assert: (self materialize: 'Date[''2012-01-01'']') = date

]

{ #category : 'tests' }
STONReaderTests >> testDateAndTime [
	| dateAndTime |
	dateAndTime := DateAndTime year: 2012 month: 1 day: 1 hour: 6 minute: 30 second: 15 offset: (Duration seconds: 60*60).
	self assert: (self materialize: 'DateAndTime[''2012-01-01T06:30:15+01:00'']') = dateAndTime

]

{ #category : 'tests' }
STONReaderTests >> testDictionary [
	| collection |
	collection := STON mapClass new at: 1 put: 1; at: 2 put: 2; yourself.
	self assert: (self materialize: '{1:1,2:2}') = collection.
	self assert: (self materialize: '{}') = STON mapClass new.

]

{ #category : 'tests' }
STONReaderTests >> testDictionaryWithComplexKeys [
	| collection reader |
	collection := STON mapClass new at: true put: 1; at: #(foo) put: 2; yourself.
	(reader := STONReader on: '{true:1,[#foo]:2}' readStream)
		allowComplexMapKeys: true.
	self assert: reader next = collection
]

{ #category : 'tests' }
STONReaderTests >> testError [
	#( 'foo' '{foo:}' '{foo,}' '[1,]' '+1' ']' '#' '' '  ' '	' 'nul' 'tru' 'fals' ) do: [ :each |
		self 
			should: [ self materialize: each ] 
			raise: STONReaderError ]

]

{ #category : 'tests' }
STONReaderTests >> testFloat [
	self assert: (self materialize: '1.5') = 1.5.
	self assert: (self materialize: '-1.5') = -1.5.
	self assert: (self materialize: '0.0') = 0.0.
false ifTrue: [ 
	self assert: (Float pi closeTo: (self materialize: '3.14149')).
	self assert: (1/3 closeTo: (self materialize: '0.333333'))].
	self assert: (self materialize: '1.0e100') = (10 raisedTo: 100) asFloat.
	self assert: (self materialize: '1.0e-100') = (10 raisedTo: -100) asFloat.
	self assert: (self materialize: '-1.0e-100') = (10 raisedTo: -100) asFloat negated.

]

{ #category : 'tests' }
STONReaderTests >> testIdentityDictionary [
	| collection |
	collection := IdentityDictionary new at: 1 put: 1; at: 2 put: 2; yourself.
	self assert: (self materialize: 'IdentityDictionary{1:1,2:2}') = collection.
	self assert: (self materialize: 'IdentityDictionary{}') = IdentityDictionary new.

]

{ #category : 'tests' }
STONReaderTests >> testInteger [
	self assert: (self materialize: '1') = 1.
	self assert: (self materialize: '-1') = -1.
	self assert: (self materialize: '0') = 0.
	self assert: (self materialize: '1234567890') = 1234567890.
	self assert: (self materialize: '-1234567890') = -1234567890
]

{ #category : 'tests' }
STONReaderTests >> testJsonString [
  "Allow double quotes for backwards JSON compatibility"

  | string |
  self assert: (self materialize: '"foo"') = 'foo'.
  self assert: (self materialize: '"FOO"') = 'FOO'.
  self
    assert:
      (self materialize: '"\u00E9l\u00E8ve en Fran\u00E7ais"') = 'élève en Français'.
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
  self assert: (self materialize: '"\"\''\\\t\r\n\f\b"') = string
]

{ #category : 'tests' }
STONReaderTests >> testList [
	| string |
	self assert: STON listClass = Array.
	self assert: (self materialize: '[1,2,3]') = (STON listClass with: 1 with: 2 with: 3).
	self assert: (self materialize: '[]') = STON listClass new
]

{ #category : 'tests' }
STONReaderTests >> testMap [
	| string |
	self assert: (self materialize: '{#foo:1}') = (STON mapClass new at: #foo put: 1; yourself).
	self assert: (self materialize: '{}') = STON mapClass new
]

{ #category : 'tests' }
STONReaderTests >> testMultiple [
	| reader |
	reader := STON reader 
		on: '123 -123 nil #foo true [ 0 ] false { #one : 1 }' readStream.
	self deny: reader atEnd.
	self assert: reader next equals: 123. 
	self assert: reader next equals: -123. 
	self assert: reader next equals: nil. 
	self assert: reader next equals: #foo. 
	self assert: reader next equals: true. 
	self assert: reader next equals: { 0 }. 
	self assert: reader next equals: false. 
	self assert: reader next equals: (Dictionary with: #one -> 1). 
	self assert: reader atEnd.

]

{ #category : 'tests' }
STONReaderTests >> testNewSymbol [
	| n notASymbol shouldBeSymbol |
	
	"Find a name that has not yet been interned"
	n := 0.
	[ (Symbol _existingWithAll: (notASymbol := 'notASymbol', n printString)) notNil ] 
		whileTrue: [ n := n + 1 ].
	"Parsing the new, not yet interned name should create a new Symbol"
	shouldBeSymbol := self materialize: '#', notASymbol.
	self assert: (shouldBeSymbol isSymbol and: [ notASymbol = shouldBeSymbol asString ])

]

{ #category : 'tests' }
STONReaderTests >> testNil [
	self assert: (self materialize: 'nil') = nil
]

{ #category : 'tests' }
STONReaderTests >> testNull [
	self assert: (self materialize: 'null') = nil
]

{ #category : 'tests' }
STONReaderTests >> testObject [
	self assert: (self materialize: 'Object{}') class == Object.

]

{ #category : 'tests' }
STONReaderTests >> testOrderedCollection [
	| collection |
	collection := OrderedCollection with: 1 with: 2 with: 3.
	self assert: (self materialize: 'OrderedCollection[1,2,3]') = collection.
	self assert: (self materialize: 'OrderedCollection[]') = OrderedCollection new.
]

{ #category : 'tests' }
STONReaderTests >> testReferenceCycle [
	| array |
	array := (self materialize: '[1,@1]').
	self assert: array class = STON listClass.
	self assert: array size = 2.
	self assert: array first = 1.
	self assert: array second == array
]

{ #category : 'tests' }
STONReaderTests >> testReferenceSharing [
	| one array |
	one := { #one }.
	array := (self materialize: '[[#one],@2,@2]').
	self assert: array = (STON listClass with: one with: one with: one).
	self assert: array first == array second.
	self assert: array first == array third
]

{ #category : 'tests' }
STONReaderTests >> testStreaming [
	| reader |
	reader := STON reader 
		on: '1 2 3 4 5 6 7 8 9 10' readStream.
	self 
		assert: (self sumOf: (Array streamContents: [ :stream |
			[ reader atEnd] whileFalse: [ 
				stream nextPut: reader next ] ]))
		equals: (self sumOf: #(1 2 3 4 5 6 7 8 9 10))

]

{ #category : 'tests' }
STONReaderTests >> testString [
  | string x |
  self assert: (self materialize: '''foo''') = 'foo'.
  self assert: (self materialize: '''FOO''') = 'FOO'.
  self
    assert:
      (x := self materialize: '''\u00E9l\u00E8ve en Fran\u00E7ais''')
        = 'élève en Français'.
  self
	assert:
		(x := self
			materialize:
				'''\u042F \u043C\u043E\u0436\u0443 \u0457\u0441\u0442\u0438 \u0441\u043A\u043B\u043E, \u0456 \u0432\u043E\u043D\u043E \u043C\u0435\u043D\u0456 \u043D\u0435 \u0437\u0430\u0448\u043A\u043E\u0434\u0438\u0442\u044C.''')
					= self unicode16TestString.
  false
    ifTrue: [ 
      "ambiguous encoding for 32-bit Unicode characters: https://github.com/svenvc/ston/issues/11"
      self
        assert:
          (x := self materialize: '''\u2338F''') = self unicode32TestString ].
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
  self assert: (self materialize: '''\"\''\\\t\r\n\f\b''') = string

]

{ #category : 'tests' }
STONReaderTests >> testSymbol [
	| string |
	self assert: (self materialize: '#''foo''') = #foo.
	self assert: (self materialize: '#foo') = #foo
]

{ #category : 'tests' }
STONReaderTests >> testTime [
	| time |
	"time := Time hour: 6 minute: 30 second: 15."
	time := Time fromSeconds: (6 * 60 *60) + (30 *60) + 15.
	self assert: (self materialize: 'Time[''06:30:15'']') = time.

]

{ #category : 'tests' }
STONReaderTests >> testUser [
	| user x |
	(user := STONTestUser new)
		username: 'john@foo.com';
		password: 'secret1'.
	self assert: (x := self materialize: 'STONTestUser{#username:''john@foo.com'',#password:''secret1'',#enabled:true}') = user

]

{ #category : 'tests' }
STONReaderTests >> testUser2 [
	| user |
	(user := STONTestUser2 new)
		username: 'john@foo.com';
		password: 'secret1'.
	self assert: (self materialize: 'STONTestUser2{#username:''john@foo.com'',#password:''secret1'',#enabled:true}') = user
]

{ #category : 'tests' }
STONReaderTests >> testWhitespace [
  | whitespace |
  whitespace := String
    withAll:
      {(Character space).
      (Character tab).
      (Character cr).
      (Character lf)}.
  self assert: (self materialize: whitespace , '123') = 123
]

{ #category : 'private' }
STONReaderTests >> unicode16TestString [
  "Cannot embed Unicode16 or Unicode32 strings in 2.x methods"

  ^ 'Ð¯ Ð¼Ð¾Ð¶Ñ ÑÑÑÐ¸ ÑÐºÐ»Ð¾, Ñ Ð²Ð¾Ð½Ð¾ Ð¼ÐµÐ½Ñ Ð½Ðµ Ð·Ð°ÑÐºÐ¾Ð´Ð¸ÑÑ.'
    decodeFromUTF8 asString "Normalize string so that it is either a Unicode string or a Legacy string"
]

{ #category : 'private' }
STONReaderTests >> unicode32TestString [
  "Cannot embed Unicode16 or Unicode32 strings in 2.x methods"

  ^ 'ð£' decodeFromUTF8
]
