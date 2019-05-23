tests
testGitDescribe

	"https://github.com/GemTalk/Rowan/issues/381#issuecomment-450502212"

	"
`git describe --match v0.0.1` can produce a version number that looks like the following v0.0.1-1-g832d2b5 ... ensure that we can use this result to compare version numbers ... the leading `-` needs to be transformed to `+`, but other than that I think that the following comparisons are correct for what we are trying to do o
	"
	| s1 s2 s3 s4 v1 v2 v3 v4 |
	s1 := '0.0.1'.
	s2 := '0.0.1+1-g832d2b5'. "git describe output .. compares equal to 0.0.1, which is acceptable - I think"
	s3 := '0.0.1+2-g59a4bdf'.	"git describe output .. compares equal to 0.0.1, which is acceptable - I think"
	s4 := '0.0.2'.

	v1 := s1 asRwSemanticVersionNumber.
	v2 := s2 asRwSemanticVersionNumber.
	v3 := s3 asRwSemanticVersionNumber.
	v4 := s4 asRwSemanticVersionNumber.

	self assert: v1 printString = s1.
	self assert: v2 printString = s2.
	self assert: v3 printString = s3.
	self assert: v4 printString = s4.

	self assert: v1 = v2.	"acceptable, I think"

	self assert: v1 = v3.	"acceptable, I think"
	self assert: v2 = v3.	"acceptable, I think"

	self assert: v1 < v4.
	self assert: v2 < v4.
	self assert: v3 < v4.

	self assert: v1 = v1.
	self assert: v2 = v2.
	self assert: v3 = v3.
	self assert: v4 = v4.