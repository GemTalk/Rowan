Class {
	#name : 'RwSemanticVersionNumberTestCase',
	#superclass : 'RwCommonVersionNumberTestCase',
	#category : 'Rowan-Tests'
}

{ #category : 'tests' }
RwSemanticVersionNumberTestCase >> sampleVersionStrings [
    ^ #('1.0.0+-' '1.0.0-alpha' '1.0.0-alpha.1' '1.0.0-0.3.7' '1.0.0-x.7.z.92')
        , #('1.0.0+build.1' '1.3.7+build.11.e0f985a')
        ,
            #('1.0.0-alpha' '1.0.0-alpha.1' '1.0.0-beta.2' '1.0.0-beta.11' '1.0.0-rc.1' '1.0.0-rc.1+build.1' '1.0.0' '1.0.0+0.3.7' '1.3.7+build' '1.3.7+build.2.b8f12d7' '1.3.7+build.11.e0f985a')
        , #('1.0.0-alp-h-a' '1.0.0-r-c.1' '1.0.0+alp-h-a' '1.0.0+r-c.1')

]

{ #category : 'tests' }
RwSemanticVersionNumberTestCase >> testEqualityComparison [
    self deny: '1.0.0+-' asSemanticVersionNumber = '1.0.0--' asSemanticVersionNumber.
    self sampleVersionStrings
        do: [ :versionString | self assert: versionString asSemanticVersionNumber = versionString asSemanticVersionNumber ]

]

{ #category : 'test matching' }
RwSemanticVersionNumberTestCase >> testInvalidSpecialMatch [
    self should: [ (self versionClass fromString: '1.1.1') match: '?.?.?' ] raise: Error

]

{ #category : 'tests' }
RwSemanticVersionNumberTestCase >> testLessThanComparison [
    self assert: '1.0.0-rc.1+build.1' asSemanticVersionNumber < '1.0.0' asSemanticVersionNumber.
    self
        assert: '1.0.0-rc.1+build.1' asSemanticVersionNumber < '1.0.0+build.0' asSemanticVersionNumber.
    self assert: '1.0.0-alpha.1' asSemanticVersionNumber < '1.0.0-0.3.7' asSemanticVersionNumber.
    self assert: '1.0.0-alpha' asSemanticVersionNumber < '1.0.0-alpha.1' asSemanticVersionNumber.
    self assert: '1.0.0+-' asSemanticVersionNumber < '1.0.0+a' asSemanticVersionNumber.
    self assert: '1.0.0-0.3.7' asSemanticVersionNumber < '1.0.0-x.7.z.92' asSemanticVersionNumber

]

{ #category : 'tests' }
RwSemanticVersionNumberTestCase >> testPrinting [
    | x vrsn |
    self sampleVersionStrings
        do: [ :versionString | self assert: versionString = (x := (vrsn := versionString asSemanticVersionNumber) printString) ]

]

{ #category : 'tests' }
RwSemanticVersionNumberTestCase >> testSemanticVersioning [
    self validateSemanticVersionStrings: #('1.0.0-rc.1+build.1' '1.0.0-rc.1+build.2' '1.0.0-rc.2+build.0').
    self validateSemanticVersionStrings: #('1.0.0-rc.1+build.1' '1.0.0+build.0').
    self validateSemanticVersionStrings: #('1.0.0-alpha' '1.0.0-1').
    self validateSemanticVersionStrings: #('1.0.0-1' '1.0.0+alpha').
    self validateSemanticVersionStrings: #('1.0.0-alpha' '1.0.0+1').
    self validateSemanticVersionStrings: #('1.0.0+alpha' '1.0.0+1').
    self validateSemanticVersionStrings: #('1.0.0--' '1.0.0-a').
    self validateSemanticVersionStrings: #('1.0.0+-' '1.0.0+a').
    self validateSemanticVersionStrings: #('1.0.0-rc.1' '1.0.0' '1.0.0+build.1').
    self validateSemanticVersionStrings: #('1.0.0-rc.1+build.1' '1.0.0-rc.1+build.2' '1.0.0-rc.2+build.3').
    self validateSemanticVersionStrings: #('1.0.0-rc.1' '1.0.0-rc.1+build.2' '1.0.0-rc.2').
    self validateSemanticVersionStrings: #('1.0.0-rc.1+build.1' '1.0.0-rc.1+build.2' '1.0.0-rc.2').
    self validateSemanticVersionStrings: #('1.0.0-rc.1+build.1' '1.0.0-rc.1+build.2' '1.0.0-rc.2+build.2').
    self validateSemanticVersionStrings: #('1.0.0-rc.1+build.1' '1.0.0' '1.0.0+build.0').
    self validateSemanticVersionStrings: #('1.0.0+10000' '1.0.0+a').
    self validateSemanticVersionStrings: #('1.0.0-10000' '1.0.0-a')

]

{ #category : 'tests' }
RwSemanticVersionNumberTestCase >> testSemanticVersioningSpecItem10 [
    "[Semantic Versioning 2.0.0-rc.1](http://semver.org/)"

    self validateSemanticVersionStrings: #('1.0.0-alpha' '1.0.0-alpha.1' '1.0.0-0.3.7' '1.0.0-x.7.z.92')

]

{ #category : 'tests' }
RwSemanticVersionNumberTestCase >> testSemanticVersioningSpecItem11 [
    "[Semantic Versioning 2.0.0-rc.1](http://semver.org/)"

    self validateSemanticVersionStrings: #('1.0.0+build.1' '1.3.7+build.11.e0f985a')

]

{ #category : 'tests' }
RwSemanticVersionNumberTestCase >> testSemanticVersioningSpecItem12 [
    "According to [Semantic Versioning 2.0.0-rc.1](http://semver.org/), the following expression should be true:
	1.0.0-alpha < 1.0.0-alpha.1 < 1.0.0-beta.2 < 1.0.0-beta.11 < 1.0.0-rc.1 < 1.0.0-rc.1+build.1 < 1.0.0 < 
		1.0.0+0.3.7 < 1.3.7+build < 1.3.7+build.2.b8f12d7 < 1.3.7+build.11.e0f985a"

    self
        validateSemanticVersionStrings:
            #('1.0.0-alpha' '1.0.0-alpha.1' '1.0.0-beta.2' '1.0.0-beta.11' '1.0.0-rc.1' '1.0.0-rc.1+build.1' '1.0.0' '1.0.0+0.3.7' '1.3.7+build' '1.3.7+build.2.b8f12d7' '1.3.7+build.11.e0f985a')

]

{ #category : 'tests' }
RwSemanticVersionNumberTestCase >> testSemanticVersioningSubsetCompliance [
    "subset of sample versions that are compatible with MetacellVersionNumber syntax"

    self
        validateSemanticVersionStrings:
            #('1.0.0-alpha' '1.0.0-alpha.1' '1.0.0-beta.2' '1.0.0-beta.11' '1.0.0-rc.1' '1.0.0')

]

{ #category : 'test matching' }
RwSemanticVersionNumberTestCase >> testSpecialMatch [
    self assert: ((self versionClass fromString: '1.1.0') match: '?').
    self assert: ((self versionClass fromString: '1.0.0-alpha') match: '?').
    self assert: ((self versionClass fromString: '1.0.0-rc.1+build.1') match: '?').
    self assert: ((self versionClass fromString: '1.1.0') match: '*.*.?').
    self assert: ((self versionClass fromString: '1.3.7+build.2.b8f12d7') match: '?').
    self assert: ((self versionClass fromString: '1.3.7+build.11.e0f985a') match: '?').
    self deny: ((self versionClass fromString: '1.1.0') match: '*.*.*-?').
    self deny: ((self versionClass fromString: '1.1.0') match: '*.*.*+?').
    self assert: ((self versionClass fromString: '1.1.1') match: '*.?')

]

{ #category : 'test matching' }
RwSemanticVersionNumberTestCase >> testStandardMatch [
    self assert: ((self versionClass fromString: '1.0.0+alpha5.0') match: '1.0.0+alpha#.0').
    self assert: ((self versionClass fromString: '1.1.1') match: '*.*.*').
    self deny: ((self versionClass fromString: '1.1.1') match: '*.*').
    self assert: ((self versionClass fromString: '1.1.0') match: '1.1.*').
    self assert: ((self versionClass fromString: '1.1.0') match: '1.#.*').
    self deny: ((self versionClass fromString: '1.10.0+build.0') match: '1.#.*').
    self assert: ((self versionClass fromString: '1.0.0-alpha5.0') match: '1.0.0-alpha#.0').
    self assert: ((self versionClass fromString: '1.0.0-alpha5+build5.0') match: '1.0.0-alpha5+build#.0').
    self deny: ((self versionClass fromString: '1.0.0-alpha10.0') match: '1.0.0-alpha#.0')

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
    self assert: (x := self versionClass fromString: '1.0.0') < (y := self versionClass fromString: '1.0.0+0').
    self deny: (x := self versionClass fromString: '1.0.0') > (y := self versionClass fromString: '1.0.0+0')

]

{ #category : 'tests' }
RwSemanticVersionNumberTestCase >> testVersion19 [
    self assert: (self versionClass fromString: '1.0.0-beta.0') < (self versionClass fromString: '1.0.0')

]

{ #category : 'tests' }
RwSemanticVersionNumberTestCase >> validateSemanticVersionStrings: versionStrings [
    | versions version |
    versions := versionStrings collect: [ :each | each asSemanticVersionNumber ].
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
