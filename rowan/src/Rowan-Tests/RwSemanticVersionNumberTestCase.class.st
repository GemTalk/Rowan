Class {
	#name : 'RwSemanticVersionNumberTestCase',
	#superclass : 'TestCase',
	#category : 'Rowan-Tests'
}

{ #category : 'Tests' }
RwSemanticVersionNumberTestCase class >> shouldInheritSelectors [

	^true

]

{ #category : 'tests' }
RwSemanticVersionNumberTestCase >> sampleVersionStrings [
    ^ #('1.0.0+-' '1.0.0-alpha' '1.0.0-alpha.1' '1.0.0-0.3.7' '1.0.0-x.7.z.92')
        , #('1.0.0+build.1' '1.3.7+build.11.e0f985a')
        ,
            #('1.0.0-alpha' '1.0.0-alpha.1' '1.0.0-beta.2' '1.0.0-beta.11' '1.0.0-rc.1' '1.0.0-rc.1+build.1' '1.0.0' '1.0.0+0.3.7' '1.3.7+build' '1.3.7+build.2.b8f12d7' '1.3.7+build.11.e0f985a')
        , #('1.0.0-alp-h-a' '1.0.0-r-c.1' '1.0.0+alp-h-a' '1.0.0+r-c.1')

]

{ #category : 'test alpha/numeric version numbers' }
RwSemanticVersionNumberTestCase >> testAlphaNumericVersion1 [

	"Use numeric comparison for pure numbers. If you non-numeric version separate with '-'"
	
	| x y |
	self assert: ((x := self versionClass fromString: '2.9.0') < (y := self versionClass fromString: '2.10.0')).

]

{ #category : 'test alpha/numeric version numbers' }
RwSemanticVersionNumberTestCase >> testAlphaNumericVersion2 [

	self assert: ((self versionClass fromString: '2.9.0-alpha.2') < (self versionClass fromString: '2.9.0-alpha.3')).

]

{ #category : 'test alpha/numeric version numbers' }
RwSemanticVersionNumberTestCase >> testAlphaNumericVersion3 [

	self assert: ((self versionClass fromString: '2.9.9-alpha.2') < (self versionClass fromString: '2.9.10')).

]

{ #category : 'test alpha/numeric version numbers' }
RwSemanticVersionNumberTestCase >> testAlphaNumericVersion4 [

	self assert: ((self versionClass fromString: '2.9.9-alpha.2') < (self versionClass fromString: '2.9.9')).

]

{ #category : 'tests' }
RwSemanticVersionNumberTestCase >> testEqualityComparison [
    self deny: '1.0.0+-' asRwSemanticVersionNumber = '1.0.0--' asRwSemanticVersionNumber.
	self assert: '1.0.0+-' asRwSemanticVersionNumber = '1.0.0+a' asRwSemanticVersionNumber.
	self sampleVersionStrings
        do: [ :versionString | self assert: versionString asRwSemanticVersionNumber = versionString asRwSemanticVersionNumber ]

]

{ #category : 'tests' }
RwSemanticVersionNumberTestCase >> testLessThanComparison [
    self assert: '1.0.0-rc.1+build.1' asRwSemanticVersionNumber < '1.0.0' asRwSemanticVersionNumber.
    self
        assert: '1.0.0-rc.1+build.1' asRwSemanticVersionNumber < '1.0.0+build.0' asRwSemanticVersionNumber.
    self assert:  '1.0.0-0.3.7' asRwSemanticVersionNumber < '1.0.0-alpha.1' asRwSemanticVersionNumber.
    self assert: '1.0.0-alpha' asRwSemanticVersionNumber < '1.0.0-alpha.1' asRwSemanticVersionNumber.
    self assert: '1.0.0-0.3.7' asRwSemanticVersionNumber < '1.0.0-x.7.z.92' asRwSemanticVersionNumber

]

{ #category : 'tests' }
RwSemanticVersionNumberTestCase >> testPrinting [
    | x vrsn |
    self sampleVersionStrings
        do: [ :versionString | self assert: versionString = (x := (vrsn := versionString asRwSemanticVersionNumber) printString) ]

]

{ #category : 'tests' }
RwSemanticVersionNumberTestCase >> testSemanticVersioning [
    self validateSemanticVersionStrings: #('1.0.0-rc.1' '1.0.0-rc.1.0' '1.0.0-rc.2').
    self validateSemanticVersionStrings: #('1.0.0-rc.1' '1.0.0').
    self validateSemanticVersionStrings: #('1.0.0-1' '1.0.0-alpha').
    self validateSemanticVersionStrings: #('1.0.0-alpha' '1.0.0+1').
    self validateSemanticVersionStrings: #('1.0.0' '1.0.1').
    self validateSemanticVersionStrings: #('1.0.0--' '1.0.0-a').
    self validateSemanticVersionStrings: #('1.0.0-rc.1' '1.0.0' '1.0.1').
    self validateSemanticVersionStrings: #('1.0.0-rc.1' '1.0.0-rc.2' '1.0.0-rc.3').
    self validateSemanticVersionStrings: #('1.0.0-10000' '1.0.0-a')

]

{ #category : 'tests' }
RwSemanticVersionNumberTestCase >> testSemanticVersioningSpecItem10 [
    "[Semantic Versioning 2.0.0-rc.1](http://semver.org/)"

    self validateSemanticVersionStrings: #( '1.0.0-0.3.7' '1.0.0-alpha' '1.0.0-alpha.1' '1.0.0-x.7.z.92')

]

{ #category : 'tests' }
RwSemanticVersionNumberTestCase >> testSemanticVersioningSubsetCompliance [
    "subset of sample versions that are compatible with MetacellVersionNumber syntax"

    self
        validateSemanticVersionStrings:
            #('1.0.0-alpha' '1.0.0-alpha.1' '1.0.0-beta.2' '1.0.0-beta.11' '1.0.0-rc.1' '1.0.0')

]

{ #category : 'tests' }
RwSemanticVersionNumberTestCase >> testVersion01 [

	self assert: ((self versionClass fromString: '1.1.1') versionString = '1.1.1')

]

{ #category : 'tests' }
RwSemanticVersionNumberTestCase >> testVersion02 [

	| v1 v2 |
	v1 := self versionClass fromString: '1.1.1'.
	v2 := self versionClass fromString: '1.0.0'.
	self assert: (v1 = v1).	
	self assert: (v2 = v2).
	self assert: (v1 > v2)

]

{ #category : 'tests' }
RwSemanticVersionNumberTestCase >> testVersion04 [

	| v1 v2 |
	v1 := self versionClass fromString: '1.0.1'.
	v2 := self versionClass fromString: '1.0.0'.
	self assert: (v1 > v2)

]

{ #category : 'tests' }
RwSemanticVersionNumberTestCase >> testVersion05 [
    | v1 v2 |
    v1 := self versionClass fromString: '3.0.0'.
    v2 := self versionClass fromString: '2.0.0'.
    self assert: v1 > v2

]

{ #category : 'tests' }
RwSemanticVersionNumberTestCase >> testVersion09 [
    | v1 v2 |
    v1 := self versionClass fromString: '1.0.0'.
    v2 := self versionClass fromString: '0.7.0'.
    self assert: v1 >= v2.
    self assert: v2 <= v1

]

{ #category : 'tests' }
RwSemanticVersionNumberTestCase >> testVersion10 [
    | x y |
    self
        assert:
            (x := (({(self versionClass fromString: '1.0.0').
            (self versionClass fromString: '0.7.0').
            (self versionClass fromString: '0.8.0').
            (self versionClass fromString: '0.9.0').
            (self versionClass fromString: '1.0.1')} sort: [ :a :b | a <= b ]) collect: [ :each | each versionString ])
                asArray) = (y := #('0.7.0' '0.8.0' '0.9.0' '1.0.0' '1.0.1'))

]

{ #category : 'tests' }
RwSemanticVersionNumberTestCase >> testVersion12 [
    self deny: (self versionClass fromString: '1.0.0') <= (self versionClass fromString: '0.7.0')

]

{ #category : 'tests' }
RwSemanticVersionNumberTestCase >> testVersion14 [

	self assert: ((self versionClass fromString: '2.9.0-alpha02') < (self versionClass fromString: '2.9.0-alpha03')).

]

{ #category : 'tests' }
RwSemanticVersionNumberTestCase >> testVersion15 [
    self assert: (self versionClass fromString: '1.0.0-beta.0') < (self versionClass fromString: '1.0.0-beta.1')

]

{ #category : 'tests' }
RwSemanticVersionNumberTestCase >> testVersion16 [
    self assert: (self versionClass fromString: '1.0.0-beta.0') < (self versionClass fromString: '1.0.0')

]

{ #category : 'tests' }
RwSemanticVersionNumberTestCase >> testVersion17 [
    self assert: (self versionClass fromString: '1.0.0') > (self versionClass fromString: '1.0.0-0').
    self assert: (self versionClass fromString: '1.0.0') > (self versionClass fromString: '1.0.0-beta.0').
    self assert: (self versionClass fromString: '1.0.0') > (self versionClass fromString: '1.0.0-beta')

]

{ #category : 'tests' }
RwSemanticVersionNumberTestCase >> testVersion18 [
    | x y |
    self deny: (x := self versionClass fromString: '1.0.0') < (y := self versionClass fromString: '1.0.0-0').
    self assert: (x := self versionClass fromString: '1.0.0') > (y := self versionClass fromString: '1.0.0-0').
    self assert: (x := self versionClass fromString: '1.0.0') = (y := self versionClass fromString: '1.0.0+0').
 
]

{ #category : 'tests' }
RwSemanticVersionNumberTestCase >> testVersion19 [
    self assert: (self versionClass fromString: '1.0.0-beta.0') < (self versionClass fromString: '1.0.0')

]

{ #category : 'tests' }
RwSemanticVersionNumberTestCase >> validateSemanticVersionStrings: versionStrings [
    | versions version |
    versions := versionStrings collect: [ :each | each asRwSemanticVersionNumber ].
    version := versions at: 1.
    2 to: versions size do: [ :index | 
        | nextVersion |
        nextVersion := versions at: index.
        self assert: version < nextVersion.
        version := nextVersion ]

]

{ #category : 'private' }
RwSemanticVersionNumberTestCase >> versionClass [
    ^ RwSemanticVersionNumber

]
