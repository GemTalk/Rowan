tests
testLessThanComparison
    self assert: '1.0.0-rc.1+build.1' asRwSemanticVersionNumber < '1.0.0' asRwSemanticVersionNumber.
    self
        assert: '1.0.0-rc.1+build.1' asRwSemanticVersionNumber < '1.0.0+build.0' asRwSemanticVersionNumber.
    self assert:  '1.0.0-0.3.7' asRwSemanticVersionNumber < '1.0.0-alpha.1' asRwSemanticVersionNumber.
    self assert: '1.0.0-alpha' asRwSemanticVersionNumber < '1.0.0-alpha.1' asRwSemanticVersionNumber.
    self assert: '1.0.0-0.3.7' asRwSemanticVersionNumber < '1.0.0-x.7.z.92' asRwSemanticVersionNumber
