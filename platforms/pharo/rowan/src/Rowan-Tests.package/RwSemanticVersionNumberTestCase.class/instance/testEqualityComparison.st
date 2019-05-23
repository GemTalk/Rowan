tests
testEqualityComparison
    self deny: '1.0.0+-' asRwSemanticVersionNumber = '1.0.0--' asRwSemanticVersionNumber.
	self assert: '1.0.0+-' asRwSemanticVersionNumber = '1.0.0+a' asRwSemanticVersionNumber.
	self sampleVersionStrings
        do: [ :versionString | self assert: versionString asRwSemanticVersionNumber = versionString asRwSemanticVersionNumber ]
