Class {
	#name : 'RwGemStoneVersionNumberTestCase',
	#superclass : 'RwCommonVersionNumberTestCase',
	#category : 'Rowan-Tests'
}

{ #category : 'test approximately greater than' }
RwGemStoneVersionNumberTestCase >> testApproxVersion01 [

	self assert: '1.1.1' asRwGemStoneVersionNumber ~> '1.1' asRwGemStoneVersionNumber

]

{ #category : 'test approximately greater than' }
RwGemStoneVersionNumberTestCase >> testApproxVersion02 [

	self deny: '1.1' asRwGemStoneVersionNumber ~> '1' asRwGemStoneVersionNumber

]

{ #category : 'test approximately greater than' }
RwGemStoneVersionNumberTestCase >> testApproxVersion03 [

	self assert: '1.1' asRwGemStoneVersionNumber ~> '1.1' asRwGemStoneVersionNumber

]

{ #category : 'tests' }
RwGemStoneVersionNumberTestCase >> testCollapseZeros [

	self assert: (RwGemStoneVersionNumber fromString: '1.0-beta.24.0.1') collapseZeros printString = '1-beta.24.0.1'.
	self assert: (RwGemStoneVersionNumber fromString: '1.0-beta.24.0.0.1') collapseZeros printString = '1-beta.24.0.0.1'.
	self assert: (RwGemStoneVersionNumber fromString: '1.0.0-beta.24.0.0.1') collapseZeros printString = '1-beta.24.0.0.1'.

]

{ #category : 'tests version increment/decrement' }
RwGemStoneVersionNumberTestCase >> testIncrementDecrementVersionNumber [
  | version |
  super testIncrementDecrementVersionNumber.
  version := '1' asRwGemStoneVersionNumber.
  self assert: version incrementMinorVersionNumber asString = '2'.
  self assert: version decrementMinorVersionNumber asString = '1'.
  version := '1.0' asRwGemStoneVersionNumber.
  self assert: version incrementMinorVersionNumber asString = '1.1'.
  self assert: version decrementMinorVersionNumber asString = '1.0'.
  version := '1.0.0' asRwGemStoneVersionNumber.
  self assert: version incrementMinorVersionNumber asString = '1.0.1'.
  self assert: version decrementMinorVersionNumber asString = '1.0.0'.
  version := '1.0.0.0' asRwGemStoneVersionNumber.
  self assert: version incrementMinorVersionNumber asString = '1.0.0.1'.
  self assert: version decrementMinorVersionNumber asString = '1.0.0.0'

]

{ #category : 'test matching' }
RwGemStoneVersionNumberTestCase >> testInvalidSpecialMatch [

	self 
		should: [ ((self versionClass fromString: '1.1') match: '?.?') ]
		raise: Error.

]

{ #category : 'test matching' }
RwGemStoneVersionNumberTestCase >> testSpecialMatch [

	self assert: ((self versionClass fromString: '1.1') match: '?').
	self assert: ((self versionClass fromString: 'alpha') match: '?').
	self assert: ((self versionClass fromString: '1.1.1.1.1') match: '?').
	self assert: ((self versionClass fromString: '1.alpha5') match: '?').
	self assert: ((self versionClass fromString: '1.1') match: '*.?').
	self assert: ((self versionClass fromString: '1.1') match: '?').
	self assert: ((self versionClass fromString: '10.1.1.1.1') match: '?').
	self deny: ((self versionClass fromString: '1.1') match: '*.*.?').
	self assert: ((self versionClass fromString: '1.1.1') match: '*.?').

]

{ #category : 'test matching' }
RwGemStoneVersionNumberTestCase >> testStandardMatch [

	self assert: ((self versionClass fromString: '1.1.1') match: '*.*.*').
	self deny: ((self versionClass fromString: '1.1.1') match: '*.*').
	self assert: ((self versionClass fromString: '1.1.0') match: '1.1.*').
	self assert: ((self versionClass fromString: '1.1.0') match: '1.#.*').
	self deny: ((self versionClass fromString: '1.10.0') match: '1.#.*').
	self assert: ((self versionClass fromString: '1-alpha5.0') match: '1-alpha#.0').
	self assert: ((self versionClass fromString: '1.alpha5.0') match: '1-alpha#.0').
	self assert: ((self versionClass fromString: '1.alpha5.0') match: '1.alpha#.0').
	self deny: ((self versionClass fromString: '1-alpha10.0') match: '1-alpha#.0').

]

{ #category : 'tests' }
RwGemStoneVersionNumberTestCase >> testVersion03 [

	| v1 v2 |
	v1 := self versionClass fromString: '1.0.0.1'.
	v2 := self versionClass fromString: '1.0.0'.
	self assert: (v1 > v2)

]

{ #category : 'tests' }
RwGemStoneVersionNumberTestCase >> testVersion05 [

	| v1 v2 |
	v1 := self versionClass fromString: '3'.
	v2 := self versionClass fromString: '2'.
	self assert: (v1 > v2)

]

{ #category : 'tests' }
RwGemStoneVersionNumberTestCase >> testVersion06 [

	| v1 v2 |
	v1 := self versionClass fromString: '3.'.
	v2 := self versionClass fromString: '2'.
	self assert: (v1 > v2)

]

{ #category : 'tests' }
RwGemStoneVersionNumberTestCase >> testVersion07 [

	| v1 v2 |
	v1 := self versionClass fromString: '3.0.0'.
	v2 := self versionClass fromString: '2'.
	self assert: (v1 > v2)

]

{ #category : 'tests' }
RwGemStoneVersionNumberTestCase >> testVersion08 [

	| v1 v2 |
	v1 := self versionClass fromString: '1.0a6'.
	v2 := self versionClass fromString: '1.0a5'.
	self assert: (v1 > v2)

]

{ #category : 'tests' }
RwGemStoneVersionNumberTestCase >> testVersion09 [

	| v1 v2 |
	v1 := self versionClass fromString: '1.0'.
	v2 := self versionClass fromString: '0.7'.
	self assert: (v1 >= v2).
	self assert: (v2 <= v1)

]

{ #category : 'tests' }
RwGemStoneVersionNumberTestCase >> testVersion10 [

	| x y |
	self assert: ((x := (({
		self versionClass fromString: '1.0'.
		self versionClass fromString: '0.7'.
		self versionClass fromString: '0.8'.
		self versionClass fromString: '0.9'.
		self versionClass fromString: '1.0.1'
	} sort: [:a :b | a <= b ]) collect: [:each | each versionString ]) asArray) = (y := #( '0.7' '0.8' '0.9' '1.0' '1.0.1')))

]

{ #category : 'tests' }
RwGemStoneVersionNumberTestCase >> testVersion11 [

	| v1 v2 |
	v1 := self versionClass fromString: '1.0.1b'.
	v2 := self versionClass fromString: '1.0.1a'.
	self assert: (v1 >= v2).
	self assert: (v2 <= v1)

]

{ #category : 'tests' }
RwGemStoneVersionNumberTestCase >> testVersion12 [

	self deny: ((self versionClass fromString: '1.0') <= (self versionClass fromString: '0.7'))

]

{ #category : 'tests' }
RwGemStoneVersionNumberTestCase >> testVersion13 [

	self deny: ((self versionClass fromString: '0.8') <= (self versionClass fromString: '0.7')).
	self deny: ((self versionClass fromString: '0.8.1.8') <= (self versionClass fromString: '0.7.0.5')).
	

]

{ #category : 'tests' }
RwGemStoneVersionNumberTestCase >> testVersion15 [

	self assert: ((self versionClass fromString: '1.0-beta.0') < (self versionClass fromString: '1.0-beta.1')).

]

{ #category : 'tests' }
RwGemStoneVersionNumberTestCase >> testVersion16 [
    self assert: (self versionClass fromString: '1.0-beta.0') < (self versionClass fromString: '1.0').
    self assert: (self versionClass fromString: '1.0-beta.0') < (self versionClass fromString: '1.0.0').
    self assert: (self versionClass fromString: '1.0-beta.0') < (self versionClass fromString: '1.0.0.0')

]

{ #category : 'tests' }
RwGemStoneVersionNumberTestCase >> testVersion17 [

	self assert: ((self versionClass fromString: '1.0') = (self versionClass fromString: '1.0.0')).
	self assert: ((self versionClass fromString: '1') = (self versionClass fromString: '1.0')).
	self assert: ((self versionClass fromString: '1') > (self versionClass fromString: '1-0')).
	self assert: ((self versionClass fromString: '1') > (self versionClass fromString: '1.0-beta.0')).
	self assert: ((self versionClass fromString: '1') > (self versionClass fromString: '1-beta.0')).
	self assert: ((self versionClass fromString: '1') > (self versionClass fromString: '1-beta')).

]

{ #category : 'tests' }
RwGemStoneVersionNumberTestCase >> testVersion18 [

	self deny: ((self versionClass fromString: '1.0') < (self versionClass fromString: '1')).
	self deny: ((self versionClass fromString: '1.0') < (self versionClass fromString: '1-0')).

]

{ #category : 'tests' }
RwGemStoneVersionNumberTestCase >> testVersion19 [

	self assert: ((self versionClass fromString: '1.0-beta.0') < (self versionClass fromString: '1')).

]

{ #category : 'tests' }
RwGemStoneVersionNumberTestCase >> testVersion20 [

	self assert: (RwGemStoneVersionNumber fromString: '') printString = ''.

]

{ #category : 'tests' }
RwGemStoneVersionNumberTestCase >> testVersion21 [

	self deny: (RwGemStoneVersionNumber fromString: '') > (RwGemStoneVersionNumber fromString: '0').
	self assert: (RwGemStoneVersionNumber fromString: '') < (RwGemStoneVersionNumber fromString: '0').
	self assert: (RwGemStoneVersionNumber fromString: '') = (RwGemStoneVersionNumber fromString: '').


]

{ #category : 'tests' }
RwGemStoneVersionNumberTestCase >> testVersion24 [

	self assert: (RwGemStoneVersionNumber fromString: '3.0.3.-rc.1') ~= (RwGemStoneVersionNumber fromString: '3.0.0.3-rc.1').
	self assert: (RwGemStoneVersionNumber fromString: '1.0.0.1.0.0') = (RwGemStoneVersionNumber fromString: '1.0.0.1').
	self assert: (RwGemStoneVersionNumber fromString: '1.0.0.1') ~= (RwGemStoneVersionNumber fromString: '1..1').


]

{ #category : 'private' }
RwGemStoneVersionNumberTestCase >> versionClass [

	^RwGemStoneVersionNumber

]
