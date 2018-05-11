"
STONWriteReadTests test serialization followed by materialization.
"
Class {
	#name : 'STONWriteReadTests',
	#superclass : 'TestCase',
	#category : 'STON-Tests'
}

{ #category : 'Testing' }
STONWriteReadTests class >> shouldInheritSelectors [
  "Me and my subclasses inherit selectors"

  ^ true
]

{ #category : 'private' }
STONWriteReadTests >> encodeOnSerialize [
  ^ false
]

{ #category : 'private' }
STONWriteReadTests >> include32BitUnicodeStrings [
  ^ false
]

{ #category : 'private' }
STONWriteReadTests >> jsonWriter [
  ^ self writer
    jsonMode: true;
    yourself
]

{ #category : 'private' }
STONWriteReadTests >> materialize: string [
  | str |
  str := string.
  self encodeOnSerialize
    ifTrue: [ str := string decodeFromUTF8 ].
  ^ self reader
    on: str readStream;
    next
]

{ #category : 'private' }
STONWriteReadTests >> prettyWriter [
  ^ self writer
    prettyPrint: true;
    yourself
]

{ #category : 'private' }
STONWriteReadTests >> reader [
  ^ STONReader new
]

{ #category : 'private' }
STONWriteReadTests >> serialize: anObject [
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
STONWriteReadTests >> serializeAndMaterialize: object [
	| serialization materialization |
	serialization := self serialize: object.
	materialization := self materialize: serialization.
	self assert: object equals: materialization
	
]

{ #category : 'private' }
STONWriteReadTests >> serializeAndMaterializeJsonMode: object [
	| serialization materialization |
	serialization := self serializeJson: object.
	materialization := self materialize: serialization.
	self assert: object equals: materialization

]

{ #category : 'private' }
STONWriteReadTests >> serializeJson: anObject [
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

{ #category : 'tests' }
STONWriteReadTests >> testAssociations [
	| associations |
	associations := OrderedCollection new.
	1 to: 10 do: [ :each |
		associations add: each -> each printString ].
	self serializeAndMaterialize: associations
]

{ #category : 'tests' }
STONWriteReadTests >> testCharacters [
	| characters |
	characters := STON listClass withAll: ($a to: $z), ($A to: $Z).
	self serializeAndMaterialize: characters
]

{ #category : 'tests' }
STONWriteReadTests >> testCollections [
  | collections |
  collections := STON listClass
    withAll:
      {#(1 2 3).
      (OrderedCollection withAll: #(1 2 3)).
      (Set withAll: #(1 2 3)).
      (IdentitySet withAll: #(1 2 3)).
      (Bag withAll: #(1 2 2 3)).
      (Dictionary new
        at: 1 put: 1;
        at: 2 put: 2;
        yourself).
      (IdentityDictionary new
        at: 1 put: 1;
        at: 2 put: 2;
        yourself).
      (#(1 2 3) asByteArray)}.
  self serializeAndMaterialize: collections
]

{ #category : 'tests' }
STONWriteReadTests >> testComplexSet [
  | collections serialization materialization |
  collections := Set
    withAll:
      {#(1 2 3).
      (OrderedCollection withAll: #(1 2 3)).
      (Set withAll: #(1 2 3)).
      (IdentitySet withAll: #(1 2 3)).
      (Bag withAll: #(1 2 2 3)).
      (Dictionary new
        at: 1 put: 1;
        at: 2 put: 2;
        yourself).
      (IdentityDictionary new
        at: 1 put: 1;
        at: 2 put: 2;
        yourself).
      (#(1 2 3) asByteArray)}.
  serialization := self serialize: collections.
  materialization := self materialize: serialization.
  collections do: [ :each | self assert: (materialization includes: each) ].
  materialization do: [ :each | self assert: (collections includes: each) ]
]

{ #category : 'tests' }
STONWriteReadTests >> testDomainObject [
	| object objects |
	object := STONTestDomainObject dummy.
	self serializeAndMaterialize: object.
	objects := STON listClass streamContents: [ :stream |
		10 timesRepeat: [ stream nextPut: STONTestDomainObject dummy ] ].
	self serializeAndMaterialize: objects.
	objects := STON mapClass new.
	10 timesRepeat: [ | newObject |
		newObject := STONTestDomainObject dummy.
		objects at: newObject integer put: newObject ].
	self serializeAndMaterialize: objects.
]

{ #category : 'tests' }
STONWriteReadTests >> testEmpty [
	| empty |
	empty := STON listClass new.
	self serializeAndMaterialize: empty.
	empty := STON mapClass new.
	self serializeAndMaterialize: empty.
]

{ #category : 'tests' }
STONWriteReadTests >> testFloats [
	| floats serialization materialization |
	floats := STON listClass withAll: ((-10 to: 10) collect: [ :each | each * Float pi ]).
	serialization := self serialize: floats.
	materialization := self materialize: serialization.
	self assert: floats size = materialization size.
	1 to: floats size do: [:index | | float |
		float := floats at: index.
		"Use #closeTo: instead of #= to increase portability"
		self assert: (float closeTo: (materialization at: index)) ]
]

{ #category : 'tests' }
STONWriteReadTests >> testJsonMode [
	| object |
	object := STON listClass withAll: {
		Float pi.
		'Hello World'.
		true.
		nil.
		STON listClass withAll: #( 1 2 3) asByteArray.
		STON mapClass new 
			at: 'x' put: 1; 
			at: 'y' put: 2; 
			yourself 
	}.
	self serializeAndMaterializeJsonMode: object

]

{ #category : 'tests' }
STONWriteReadTests >> testPrimitives [
	| primitives |
	primitives := STON listClass withAll: { true. false. nil }.
	self serializeAndMaterialize: primitives
]

{ #category : 'tests' }
STONWriteReadTests >> testSmallIntegers [
	| integers |
	integers := STON listClass withAll: (-10 to: 10).
	self serializeAndMaterialize: integers
]

{ #category : 'tests' }
STONWriteReadTests >> testStrings [
	| strings |
	strings := Collection allSubclasses collect: [ :each | each name asString ].
	self serializeAndMaterialize: strings
]

{ #category : 'tests' }
STONWriteReadTests >> testSymbols [
	| symbols |
	self serializeAndMaterialize: #( #bytes #'' ).
	symbols := Collection allSubclasses collect: [ :each | each name ].
	self serializeAndMaterialize: symbols

]

{ #category : 'tests' }
STONWriteReadTests >> testUnicodeStrings [
  | strings |
  strings := {(String
    withAll:
      {(Character codePoint: 0).
      (Character codePoint: 255).
      (Character codePoint: 256)}).
  (self unicode16TestString).
  (self unicode32TestString).
  'élève en Français'}.
  self serializeAndMaterialize: strings
]

{ #category : 'tests' }
STONWriteReadTests >> testUser [
	| user users |
	user := STONTestUser dummy.
	self serializeAndMaterialize: user.
	users := STON listClass streamContents: [ :stream |
		10 timesRepeat: [ stream nextPut: STONTestUser dummy ] ].
	self serializeAndMaterialize: users.
	users := STON mapClass new.
	10 timesRepeat: [ | newUser |
		newUser := STONTestUser dummy.
		users at: newUser username put: newUser ].
	self serializeAndMaterialize: users.
]

{ #category : 'tests' }
STONWriteReadTests >> testUser2 [
	| user users |
	user := STONTestUser2 dummy.
	self serializeAndMaterialize: user.
	users := STON listClass streamContents: [ :stream |
		10 timesRepeat: [ stream nextPut: STONTestUser2 dummy ] ].
	self serializeAndMaterialize: users.
	users := STON mapClass new.
	10 timesRepeat: [ | newUser |
		newUser := STONTestUser2 dummy.
		users at: newUser username put: newUser ].
	self serializeAndMaterialize: users.
]

{ #category : 'private' }
STONWriteReadTests >> unicode16TestString [
  "Cannot embed Unicode16 or Unicode32 strings in 2.x methods"

  ^ 'Ð¯ Ð¼Ð¾Ð¶Ñ ÑÑÑÐ¸ ÑÐºÐ»Ð¾, Ñ Ð²Ð¾Ð½Ð¾ Ð¼ÐµÐ½Ñ Ð½Ðµ Ð·Ð°ÑÐºÐ¾Ð´Ð¸ÑÑ.'
    decodeFromUTF8
]

{ #category : 'private' }
STONWriteReadTests >> unicode32TestString [
  "Cannot embed Unicode16 or Unicode32 strings in 2.x methods"

  ^ self include32BitUnicodeStrings
    ifTrue: [ 'ð£' decodeFromUTF8 ]
    ifFalse: [ 'abc' ]
]

{ #category : 'private' }
STONWriteReadTests >> writer [
  ^ STONWriter new
]
