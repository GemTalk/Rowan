"
The tests in this class were extracted from the examples in Semantic Version Specification ...
"
Class {
	#name : 'RwSemanticVersionNumber200TestCase',
	#superclass : 'TestCase',
	#category : 'Rowan-Tests'
}

{ #category : 'tests' }
RwSemanticVersionNumber200TestCase >> testSpec_02 [

	"
A normal version number MUST take the form X.Y.Z where X, Y, and Z are
non-negative integers, and MUST NOT contain leading zeroes. X is the
major version, Y is the minor version, and Z is the patch version.
Each element MUST increase numerically. For instance: 1.9.0 -> 1.10.0 -> 1.11.0.
	"
	| s1 s2 s3 v1 v2 v3 |
	s1 := '1.9.0' .
	s2 := '1.10.0' .
	s3 := '1.11.0' .

	v1 := s1 asRwSemanticVersionNumber.
	v2 := s2 asRwSemanticVersionNumber.
	v3 := s3 asRwSemanticVersionNumber.

	self assert: v1 printString = s1.
	self assert: v2 printString = s2.
	self assert: v3 printString = s3.

	self assert: v1 < v2.
	self assert: v2 < v3.
	self assert: v1 < v3.

	self assert: v1 = v1.
	self assert: v2 = v2.
	self assert: v3 = v3.

]

{ #category : 'tests' }
RwSemanticVersionNumber200TestCase >> testSpec_09 [

	"
A pre-release version MAY be denoted by appending a hyphen and a
series of dot separated identifiers immediately following the patch
version. Identifiers MUST comprise only ASCII alphanumerics and hyphen
[0-9A-Za-z-]. Identifiers MUST NOT be empty. Numeric identifiers MUST
NOT include leading zeroes. Pre-release versions have a lower
precedence than the associated normal version. A pre-release version
indicates that the version is unstable and might not satisfy the
intended compatibility requirements as denoted by its associated
normal version. Examples: 1.0.0-alpha, 1.0.0-alpha.1, 1.0.0-0.3.7,
1.0.0-x.7.z.92.
	"
	| s1 s2 s3 s4 s5 v1 v2 v3 v4 v5 |
	s1 := '1.0.0-alpha' .
	s2 := '1.0.0-alpha.1' .
	s3 := '1.0.0-0.3.7' .
	s4 := '1.0.0-x.7.z.92' .

	s5 := '1.0.0' .

	v1 := s1 asRwSemanticVersionNumber.
	v2 := s2 asRwSemanticVersionNumber.
	v3 := s3 asRwSemanticVersionNumber.
	v4 := s4 asRwSemanticVersionNumber.
	v5 := s5 asRwSemanticVersionNumber.

	self assert: v1 printString = s1.
	self assert: v2 printString = s2.
	self assert: v3 printString = s3.
	self assert: v4 printString = s4.

	self assert: v1 < v5.
	self assert: v2 < v5.
	self assert: v3 < v5.
	self assert: v4 < v5.

	self assert: v1 = v1.
	self assert: v2 = v2.
	self assert: v3 = v3.
	self assert: v4 = v4.
	self assert: v5 = v5.

]

{ #category : 'tests' }
RwSemanticVersionNumber200TestCase >> testSpec_10 [

	"
Build metadata MAY be denoted by appending a plus sign and a series of dot
separated identifiers immediately following the patch or pre-release version.
Identifiers MUST comprise only ASCII alphanumerics and hyphen [0-9A-Za-z-].
Identifiers MUST NOT be empty. Build metadata MUST be ignored when determining
version precedence. Thus two versions that differ only in the build metadata,
have the same precedence. Examples: 1.0.0-alpha+001, 1.0.0+20130313144700,
1.0.0-beta+exp.sha.5114f85.
	"

	| s1 s2 s3 s4 v1 v2 v3 v4 |
	s1 := '1.0.0-alpha+001' .
	s2 := '1.0.0-beta+exp.sha.5114f85' .
	s3 := '1.0.0+20130313144700' .
	s4 := '1.0.0+99999999999999' .

	v1 := s1 asRwSemanticVersionNumber.
	v2 := s2 asRwSemanticVersionNumber.
	v3 := s3 asRwSemanticVersionNumber.
	v4 := s4 asRwSemanticVersionNumber.

	self assert: v1 printString = s1.
	self assert: v2 printString = s2.
	self assert: v3 printString = s3.
	self assert: v4 printString = s4.

	self assert: v1 < v2.
	self assert: v2 < v3.
	self assert: v1 < v3.
	self assert: v1 < v4.

	self assert: v1 = v1.
	self assert: v2 = v2.
	self assert: v3 = v3.
	self assert: v4 = v4.
	self assert: v4 = v3.

]

{ #category : 'tests' }
RwSemanticVersionNumber200TestCase >> testSpec_11 [

	"
Precedence refers to how versions are compared to each other when ordered.
Precedence MUST be calculated by separating the version into major, minor, patch
and pre-release identifiers in that order (Build metadata does not figure
into precedence). Precedence is determined by the first difference when
comparing each of these identifiers from left to right as follows: Major, minor,
and patch versions are always compared numerically. Example: 1.0.0 < 2.0.0 <
2.1.0 < 2.1.1. When major, minor, and patch are equal, a pre-release version has
lower precedence than a normal version. Example: 1.0.0-alpha < 1.0.0. Precedence
for two pre-release versions with the same major, minor, and patch version MUST
be determined by comparing each dot separated identifier from left to right
until a difference is found as follows: identifiers consisting of only digits
are compared numerically and identifiers with letters or hyphens are compared
lexically in ASCII sort order. Numeric identifiers always have lower precedence
than non-numeric identifiers. A larger set of pre-release fields has a higher
precedence than a smaller set, if all of the preceding identifiers are equal.
Example: 1.0.0-alpha < 1.0.0-alpha.1 < 1.0.0-alpha.beta < 1.0.0-beta <
1.0.0-beta.2 < 1.0.0-beta.11 < 1.0.0-rc.1 < 1.0.0.
	"

	| vrsns vrsna vrsnb |
	vrsns := #( '1.0.0-alpha' '1.0.0-alpha.1' '1.0.0-alpha.beta' '1.0.0-beta' '1.0.0-beta.2' '1.0.0-beta.11' '1.0.0-rc.1' '1.0.0').
	vrsns
		do: [:str |
			vrsnb := str asRwSemanticVersionNumber.
			self assert: vrsnb printString = str.
			self assert: vrsnb = vrsnb.
			vrsna ifNotNil: [ self assert: vrsna < vrsnb ].
			vrsna := vrsnb ].
	vrsna := nil.
	vrsns reverse
		do: [:str |
			vrsnb := str asRwSemanticVersionNumber.
			self assert: vrsnb printString = str.
			self assert: vrsnb = vrsnb.
			vrsna ifNotNil: [ self assert: vrsna > vrsnb ].
			vrsna := vrsnb ].

	self deny: '1.0.0-alpha.beta' asRwSemanticVersionNumber < '1.0.0-alpha.1' asRwSemanticVersionNumber

]
