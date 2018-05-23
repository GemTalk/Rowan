Class {
	#name : 'RwGemStoneVersionNumberTestCase',
	#superclass : 'TestCase',
	#category : 'Rowan-Tests'
}

{ #category : 'Tests' }
RwGemStoneVersionNumberTestCase class >> shouldInheritSelectors [

	^true

]

{ #category : 'test alpha/numeric version numbers' }
RwGemStoneVersionNumberTestCase >> testAlphaNumericVersion1 [

	"Use numeric comparison for pure numbers. If you non-numeric version separate with '-'"
	
	| x y |
	self assert: ((x := self versionClass fromString: '2.9.0') < (y := self versionClass fromString: '2.10.0')).

]

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

{ #category : 'test approximately greater than' }
RwGemStoneVersionNumberTestCase >> testApproxVersion04 [

	self assert: '3.2.0' asRwGemStoneVersionNumber approximateBase asString = '3.3'.
	self assert: '3.2' asRwGemStoneVersionNumber approximateBase asString = '3.3'.

]

{ #category : 'tests' }
RwGemStoneVersionNumberTestCase >> testCollapseZeros [

	self assert: (RwGemStoneVersionNumber fromString: '1.0') collapseZeros printString = '1'.
	self assert: (RwGemStoneVersionNumber fromString: '1.0') collapseZeros printString = '1'.
	self assert: (RwGemStoneVersionNumber fromString: '1.0.0') collapseZeros printString = '1'.

]

{ #category : 'tests' }
RwGemStoneVersionNumberTestCase >> testVersion01 [

	self assert: ((self versionClass fromString: '1.1.1') versionString = '1.1.1')

]

{ #category : 'tests' }
RwGemStoneVersionNumberTestCase >> testVersion02 [

	| v1 v2 |
	v1 := self versionClass fromString: '1.1.1'.
	v2 := self versionClass fromString: '1.0.0'.
	self assert: (v1 = v1).	
	self assert: (v2 = v2).
	self assert: (v1 > v2)

]

{ #category : 'tests' }
RwGemStoneVersionNumberTestCase >> testVersion03 [

	| v1 v2 |
	v1 := self versionClass fromString: '1.0.0.1'.
	v2 := self versionClass fromString: '1.0.0'.
	self assert: (v1 > v2)

]

{ #category : 'tests' }
RwGemStoneVersionNumberTestCase >> testVersion04 [

	| v1 v2 |
	v1 := self versionClass fromString: '1.0.1'.
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
RwGemStoneVersionNumberTestCase >> testVersion17 [

	self assert: ((self versionClass fromString: '1.0') = (self versionClass fromString: '1.0.0')).
	self assert: ((self versionClass fromString: '1') = (self versionClass fromString: '1.0')).

]

{ #category : 'tests' }
RwGemStoneVersionNumberTestCase >> testVersion18 [

	self deny: ((self versionClass fromString: '1.0') < (self versionClass fromString: '1')).
	self deny: ((self versionClass fromString: '1.0') < (self versionClass fromString: '1-0')).

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

	self assert: (RwGemStoneVersionNumber fromString: '1.0.0.1.0.0') = (RwGemStoneVersionNumber fromString: '1.0.0.1').
	self assert: (RwGemStoneVersionNumber fromString: '1.0.0.1') ~= (RwGemStoneVersionNumber fromString: '1..1').

]

{ #category : 'private' }
RwGemStoneVersionNumberTestCase >> versionClass [

	^RwGemStoneVersionNumber

]
