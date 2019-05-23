tests
testSpec_10

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
