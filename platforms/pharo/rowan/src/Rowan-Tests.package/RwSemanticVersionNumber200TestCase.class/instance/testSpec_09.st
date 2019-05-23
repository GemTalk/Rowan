tests
testSpec_09

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
