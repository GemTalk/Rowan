Class {
	#name : 'RwCommonVersionNumberTestCase',
	#superclass : 'TestCase',
	#category : 'Rowan-Tests'
}

{ #category : 'testing' }
RwCommonVersionNumberTestCase class >> isAbstract [
    "Override to true if a TestCase subclass is Abstract and should not have
	TestCase instances built from it"

    ^ self name = #'RwCommonVersionNumberTestCase'

]

{ #category : 'private' }
RwCommonVersionNumberTestCase >> defaultVersion1 [
  ^ self versionClass fromString: '1.0.0'

]

{ #category : 'test alpha/numeric version numbers' }
RwCommonVersionNumberTestCase >> testAlphaNumericVersion1 [

	"Use numeric comparison for pure numbers. If you non-numeric version separate with '-'"
	
	| x y |
	self assert: ((x := self versionClass fromString: '2.9.0') < (y := self versionClass fromString: '2.10.0')).

]

{ #category : 'test alpha/numeric version numbers' }
RwCommonVersionNumberTestCase >> testAlphaNumericVersion2 [

	self assert: ((self versionClass fromString: '2.9.0-alpha.2') < (self versionClass fromString: '2.9.0-alpha.3')).

]

{ #category : 'test alpha/numeric version numbers' }
RwCommonVersionNumberTestCase >> testAlphaNumericVersion3 [

	self assert: ((self versionClass fromString: '2.9.9-alpha.2') < (self versionClass fromString: '2.9.10')).

]

{ #category : 'test alpha/numeric version numbers' }
RwCommonVersionNumberTestCase >> testAlphaNumericVersion4 [

	self assert: ((self versionClass fromString: '2.9.9-alpha.2') < (self versionClass fromString: '2.9.9')).

]

{ #category : 'tests version increment/decrement' }
RwCommonVersionNumberTestCase >> testIncrementDecrementVersion [
  | version |
  version := self defaultVersion1.
  self assert: version incrementMajorVersion asString = '2.0.0'.
  self assert: version decrementMajorVersion asString = '1.0.0'.
  self assert: version incrementMinorVersion asString = '1.1.0'.
  self assert: version decrementMinorVersion asString = '1.0.0'.
  self assert: version incrementPatchVersion asString = '1.0.1'.
  self assert: version decrementPatchVersion asString = '1.0.0'

]

{ #category : 'tests version increment/decrement' }
RwCommonVersionNumberTestCase >> testIncrementDecrementVersionNumber [
  | version |
  version := self defaultVersion1.
  self assert: version incrementMinorVersionNumber asString = '1.0.1'.
  self assert: version decrementMinorVersionNumber asString = '1.0.0'

]

{ #category : 'tests' }
RwCommonVersionNumberTestCase >> testVersion01 [

	self assert: ((self versionClass fromString: '1.1.1') versionString = '1.1.1')

]

{ #category : 'tests' }
RwCommonVersionNumberTestCase >> testVersion02 [

	| v1 v2 |
	v1 := self versionClass fromString: '1.1.1'.
	v2 := self versionClass fromString: '1.0.0'.
	self assert: (v1 = v1).	
	self assert: (v2 = v2).
	self assert: (v1 > v2)

]

{ #category : 'tests' }
RwCommonVersionNumberTestCase >> testVersion04 [

	| v1 v2 |
	v1 := self versionClass fromString: '1.0.1'.
	v2 := self versionClass fromString: '1.0.0'.
	self assert: (v1 > v2)

]

{ #category : 'tests' }
RwCommonVersionNumberTestCase >> testVersion14 [

	self assert: ((self versionClass fromString: '2.9.0-alpha02') < (self versionClass fromString: '2.9.0-alpha03')).

]

{ #category : 'private' }
RwCommonVersionNumberTestCase >> versionClass [
    ^ self subclassResponsibility

]
