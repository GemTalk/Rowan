tests
testSpec_02

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
