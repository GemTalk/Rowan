smalltalk api
testSuiteForProjectNamed: projectName

	| suite |
	suite := TestSuite named: projectName , ' tests'.
	self _addTestsForProjectNamed: projectName toTestSuite: suite.
	^ suite
