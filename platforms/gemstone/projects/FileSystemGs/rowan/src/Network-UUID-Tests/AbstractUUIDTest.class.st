Class {
	#name : 'AbstractUUIDTest',
	#superclass : 'TestCase',
	#instVars : [
		'currentUuidGeneratorClass'
	],
	#category : 'Network-UUID-Tests'
}

{ #category : 'Testing' }
AbstractUUIDTest class >> isAbstract [
  "Override to true if a TestCase subclass is Abstract and should not have
	TestCase instances built from it"

  ^ self sunitName = #'AbstractUUIDTest'
]

{ #category : 'running' }
AbstractUUIDTest >> generatorClass [
  self subclassResponsibility
]

{ #category : 'running' }
AbstractUUIDTest >> setUp [
  super setUp.
  currentUuidGeneratorClass := UUID _generatorClass.
  UUID generatorClass: self generatorClass
]

{ #category : 'running' }
AbstractUUIDTest >> tearDown [
  super tearDown.
  UUID generatorClass: currentUuidGeneratorClass
]

{ #category : 'tests' }
AbstractUUIDTest >> testComparison [
  "self debug: #testComparison"

  | a b |
  a := UUID fromString: '0608b9dc-02e4-4dd0-9f8a-ea45160df641'.
  b := UUID fromString: 'e85ae7ba-3ca3-4bae-9f62-cc2ce51c525e'.
  self assert: a < b.
  self deny: a > b.
  a := UUID fromString: '0608b9dc-02e4-4dd0-9f8a-ea45160df641'.
  b := UUID fromString: '0608b9dc-02e4-4dd0-9f8a-ea45160df642'.
  self assert: a < b.
  a := UUID fromString: '0608b9dc-02e4-4dd0-9f8a-ea45160df641'.
  b := UUID fromString: '0608b9dc-02e4-4dd0-9f8a-ea45160df640'.
  self assert: a > b
]

{ #category : 'tests' }
AbstractUUIDTest >> testComparisonA [
  | a b |
  a := UUID fromString: '0608b9dc-02e4-4dd0-9f8a-ea45160df641'.
  b := UUID fromString: 'e85ae7ba-3ca3-4bae-9f62-cc2ce51c525e'.
  self assert: a = a copy.
  self assert: b = b copy.
  self assert: a < b.
  self assert: b > a.
  self deny: a > b = (b > a)
]

{ #category : 'tests' }
AbstractUUIDTest >> testCreation [
  | uuid |
  uuid := UUID new.
  self should: [ uuid size = 16 ].
  self shouldnt: [ uuid isNilUUID ].
  self should: [ uuid asString size = 36 ]
]

{ #category : 'tests' }
AbstractUUIDTest >> testCreationEquality [
  | uuid1 uuid2 |
  uuid1 := UUID new.
  uuid2 := UUID new.
  self should: [ uuid1 = uuid1 ].
  self should: [ uuid2 = uuid2 ].
  self shouldnt: [ uuid1 = uuid2 ].
  self shouldnt: [ uuid1 hash = uuid2 hash ]
]

{ #category : 'tests' }
AbstractUUIDTest >> testCreationFromString [
  | uuid string |
  string := UUID nilUUID asString.
  uuid := UUID fromString: string.
  self should: [ uuid size = 16 ].
  self should: [ uuid = UUID nilUUID ].
  self should: [ uuid isNilUUID ].
  self should: [ uuid asString size = 36 ].
  self should: [ uuid asArray asSet size = 1 ].
  self should: [ (uuid asArray asSet asArray at: 1) = 0 ]
]

{ #category : 'tests' }
AbstractUUIDTest >> testCreationFromStringNotNil [
  | uuid string |
  string := UUID new asString.
  uuid := UUID fromString: string.
  self should: [ uuid size = 16 ].
  self should: [ uuid asString size = 36 ]
]

{ #category : 'tests' }
AbstractUUIDTest >> testCreationNil [
  | uuid |
  uuid := UUID nilUUID.
  self should: [ uuid size = 16 ].
  self should: [ uuid isNilUUID ].
  self should: [ uuid asString size = 36 ].
  self should: [ uuid asArray asSet size = 1 ].
  self should: [ (uuid asArray asSet asArray at: 1) = 0 ]
]

{ #category : 'tests' }
AbstractUUIDTest >> testCreationNodeBased [
  (UUID new asString copyLast: 12) = (UUID new asString copyLast: 12)
    ifFalse: [ ^ self ].
  1000
    timesRepeat: [ 
      | uuid |
      uuid := UUID new.
      self should: [ ((uuid at: 7) bitAnd: 16rF0) = 16r10 ].
      self should: [ ((uuid at: 9) bitAnd: 16rC0) = 16r80 ] ]

]

{ #category : 'tests' }
AbstractUUIDTest >> testDuplicationsKinda [
  | check size |
  size := 5000.
  check := Set new: size.
  size
    timesRepeat: [ 
      | uuid |
      uuid := UUID new.
      self shouldnt: [ check includes: uuid ].
      check add: uuid ]
]

{ #category : 'tests' }
AbstractUUIDTest >> testOrder [
  100
    timesRepeat: [ 
      | uuid1 uuid2 |
      uuid1 := UUID new.
      uuid2 := UUID new.
      (uuid1 asString copyLast: 12) = (uuid2 asString copyLast: 12)
        ifTrue: [ 
          self should: [ uuid1 < uuid2 ].
          self should: [ uuid2 > uuid1 ].
          self shouldnt: [ uuid1 = uuid2 ] ] ]

]

{ #category : 'tests' }
AbstractUUIDTest >> testUniqueness [
  "Test uniqueness for a given number of generated values"

  | maxTestValue |
  maxTestValue := 1000.
  self
    assert:
      ((1 to: maxTestValue) collect: [ :i | UUID new asString ]) asSet size
        = maxTestValue
]
